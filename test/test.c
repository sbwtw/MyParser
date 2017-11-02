/**
 * @file account.c Account API
 * @ingroup core
 */

#include "xmlnode.h"

typedef struct
{
	PurpleConnectionErrorInfo *current_error;

	/* libpurple 3.0.0 compatibility */
	char *password_keyring;
	char *password_mode;
	char *password_ciphertext;
} PurpleAccountPrivate;

#define PURPLE_ACCOUNT_GET_PRIVATE(account) \
	((PurpleAccountPrivate *) (account->priv))

/* TODO: Should use PurpleValue instead of this?  What about "ui"? */
typedef struct
{
	PurplePrefType type;

	char *ui;

	union
	{
		int integer;
		char *string;
		gboolean boolean;

	} value;

} PurpleAccountSetting;

static PurpleAccountUiOps *account_ui_ops = NULL;

static void
_purple_account_set_encrypted_password(PurpleAccount *account, const char *keyring,
	const char *mode, const char *ciphertext);
static gboolean
_purple_account_get_encrypted_password(PurpleAccount *account, const char **keyring,
	const char **mode, const char **ciphertext);
static gboolean

/*********************************************************************
 * Writing to disk                                                   *
 *********************************************************************/

static void
setting_to_xmlnode(gpointer key, gpointer value, gpointer user_data)
{
	const char *name;
	PurpleAccountSetting *setting;
	xmlnode *node, *child;
	char buf[21];

	name    = (const char *)key;
	setting = (PurpleAccountSetting *)value;
	node    = (xmlnode *)user_data;

	child = xmlnode_new_child(node, "setting");
	xmlnode_set_attrib(child, "name", name);

	if (setting->type == PURPLE_PREF_INT) {
		xmlnode_set_attrib(child, "type", "int");
		g_snprintf(buf, sizeof(buf), "%d", setting->value.integer);
		xmlnode_insert_data(child, buf, -1);
	}
	else if (setting->type == PURPLE_PREF_STRING && setting->value.string != NULL) {
		xmlnode_set_attrib(child, "type", "string");
		xmlnode_insert_data(child, setting->value.string, -1);
	}
	else if (setting->type == PURPLE_PREF_BOOLEAN) {
		xmlnode_set_attrib(child, "type", "bool");
		g_snprintf(buf, sizeof(buf), "%d", setting->value.boolean);
		xmlnode_insert_data(child, buf, -1);
	}
}

