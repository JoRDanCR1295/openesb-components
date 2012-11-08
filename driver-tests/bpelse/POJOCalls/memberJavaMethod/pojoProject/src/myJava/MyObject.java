package myJava;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

public class MyObject {

    /**
     * @param args
     * @throws ClassNotFoundException
     * @throws NoSuchMethodException
     * @throws SecurityException
     * @throws InvocationTargetException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     */
    public static void main(String[] args) throws ClassNotFoundException,
            SecurityException, NoSuchMethodException, IllegalArgumentException,
            IllegalAccessException, InvocationTargetException {
        MyObject obj = new MyObject();

        System.out.println("object method excecution: "
                + obj.exampleMemberConcat("first string", ",second string"));

        Class targetClass = Class.forName("myJava.MyObject", true, Thread
                .currentThread().getContextClassLoader());
        
        Method method = targetClass.getDeclaredMethod("exampleMemberConcat", new Class[] { String.class, String.class });

        //Method method = targetClass.getDeclaredMethod("exampleStaticConcat", new Class[] { String.class, String.class });

        if(Modifier.isStatic(method.getModifiers())){
            System.out.println("object method excecution: "
                    + method.invoke(null, new Object[] { "first string",
                    ",second string" }));
        } else {
            System.out.println("object method excecution: "
                    + method.invoke(obj, new Object[] { "first string",
                    ",second string" }));
        }
    }

    public String exampleMemberConcat(String str1, String str2) {
        return str1 + str2;
    }

    public static String exampleStaticConcat(String str1, String str2) {
        return str1 + str2;
    }

}
