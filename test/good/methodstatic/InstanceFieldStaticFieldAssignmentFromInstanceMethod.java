public class InstanceFieldStaticFieldAssignmentFromInstanceMethod
{
  private InstanceFieldStaticFieldAssignmentFromInstanceMethod x = new InstanceFieldStaticFieldAssignmentFromInstanceMethod();
  public static int y;
  private void inst()
  {
    x.y = 1;
  }
}