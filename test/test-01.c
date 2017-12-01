
void func(int a, double b);

struct S { int a; };

void func(int a, double b)
{
    b = 1;

    if (a == 2 || a <= 1)
        a = 1;
}
