public class InstanceMethodFromInstanceField
{
  private int x = inst();
  private int inst()
  {
    return 1;
  }
}