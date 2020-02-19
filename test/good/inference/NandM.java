public class NandM
{
  static final private se.chalmers.paragon.Lock Foo = se.chalmers.paragon.Lock.newLock("Foo", 0);
  int m(int p)
  {
    return p;
  }
  int n(int x)
  {
    return m(x);
  }
}