public class StaticMethodFromInstanceField
{
  private int x = stat();
  private static int stat()
  {
    return 1;
  }
}