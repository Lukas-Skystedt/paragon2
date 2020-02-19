public class ReturnOpenBlock
{
  static final private se.chalmers.paragon.Lock Lock = se.chalmers.paragon.Lock.newLock("Lock", 0);
  public int f()
  {
    {
      return 1;
    }
  }
}