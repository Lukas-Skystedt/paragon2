public class InstanceMethodStaticFieldFromInstanceField
{
  private int y = inst().x;
  public static int x = 1;
  private InstanceMethodStaticFieldFromInstanceField inst()
  {
    return new InstanceMethodStaticFieldFromInstanceField();
  }
}