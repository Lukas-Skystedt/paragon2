public class StaticMethodInstanceMethodFromStaticMethod
{
  private static void stat()
  {
    int x = stat1().inst();
  }
  private int inst()
  {
    return 1;
  }
  public static StaticMethodInstanceMethodFromStaticMethod stat1()
  {
    return new StaticMethodInstanceMethodFromStaticMethod();
  }
}