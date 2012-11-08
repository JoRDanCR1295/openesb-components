/*
 * Base64Impl.java
 * 
 * Created on Oct 8, 2007, 4:40:39 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.databasebc.security;

/**
 *
 * @author narayan
 */
public class Base64Impl implements Base64{
    
    private Base64Impl(){
        
    }
    
    private static Base64Impl instance = null;
    
    public static final Base64Impl getInstance(){
        if(instance==null)
            instance = new Base64Impl();
        return instance;
    }
    
    public String decode(String data) {
        return getUTF8String(com.sun.org.apache.xerces.internal.impl.dv.util.Base64.decode(data));
    }

    public String encode(String data) {
        String encoded = com.sun.org.apache.xerces.internal.impl.dv.util.Base64.encode(getUTF8Bytes(data));
        //This encoder adds a new line character at the end of the base64 encoded data, so
        //remove this character before returning.
        if (encoded != null) {
            //encoded = encoded.substring(0, encoded.length() - 1);
        }
        
        return encoded;
    }
        
    public static byte[] getUTF8Bytes(String data) {
        if (data == null) {
            return new byte[0];
        }
        
        try {
            return data.getBytes("UTF-8");
        } catch (java.io.UnsupportedEncodingException uee) {
            // Default encoding if UTF-8 is not available.
            return data.getBytes();
        }
    }

    public static String getUTF8String(byte [] data) {
        try {
            return new String(data, "UTF-8");
        } catch (java.io.UnsupportedEncodingException uee) {
            // default encoding ...
            return new String(data);
        }
    }
}
