import java.util.Scanner;

public class WrapperTestsExpr {

    public static void main(String[] args) {

    	// should return (3 - 2) * (6 - (12 + 2))

    	//  		   =    1   *    - 8 =  -8

        int returnValue = TestsExpr.test(9,3,6,12);
        System.out.println("result: "+ returnValue);

    }

}
