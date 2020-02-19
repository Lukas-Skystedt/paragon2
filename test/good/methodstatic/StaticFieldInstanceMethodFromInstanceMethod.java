public class StaticFieldInstanceMethodFromInstanceMethod
{
  private static java.lang.Object obj = new java.lang.Object();
  private void inst()
  {
    java.lang.String str = obj.toString();
  }
}