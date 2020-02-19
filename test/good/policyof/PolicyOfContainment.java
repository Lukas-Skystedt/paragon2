class PolicyOfContainment
{
  int t = 0;
  int b = 1;
  int foo(int x)
  {
    return x;
  }
  void bar()
  {
    t = foo(4);
  }
  int baz(int x, int y)
  {
    return x + y;
  }
}