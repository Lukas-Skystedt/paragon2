public class StaticMethodInstanceFieldFromStaticField
{
  private static int y = stat().x;
  public int x = 1;
  private static StaticMethodInstanceFieldFromStaticField stat()
  {
    return new StaticMethodInstanceFieldFromStaticField();
  }
}