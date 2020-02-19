import se.chalmers.paragon.*;
public class BasicArrayPolicies
{
  public static final java.lang.Object alice = new java.lang.Object();
  public static final se.chalmers.paragon.Policy apol = se.chalmers.paragon.Policy.newPolicy("apol", se.chalmers.paragon.Policy.newPClause(alice));
  public int[] myArray;
  public void foo()
  {
    int j = myArray[0];
  }
}