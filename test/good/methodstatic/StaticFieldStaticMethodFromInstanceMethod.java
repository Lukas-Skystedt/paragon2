public class StaticFieldStaticMethodFromInstanceMethod
{
  private static java.lang.Integer i = new java.lang.Integer(1);
  private void inst()
  {
    java.lang.String str = i.toString(1);
  }
}