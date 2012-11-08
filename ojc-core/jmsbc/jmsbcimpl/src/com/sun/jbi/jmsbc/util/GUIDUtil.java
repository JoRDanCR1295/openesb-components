package com.sun.jbi.jmsbc.util;

/**
*
* @author Sun
*/
final public class GUIDUtil {
   
   public static String generateGUID() {
       return java.util.UUID.randomUUID().toString();
   }
}
