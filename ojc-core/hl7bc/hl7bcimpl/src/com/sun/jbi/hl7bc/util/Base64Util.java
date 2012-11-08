package com.sun.jbi.hl7bc.util;

/**
 *
 * @author narayan
 */
public class Base64Util{
    
    

    

    public String encode(byte[] data){
    	String encoded = com.sun.org.apache.xerces.internal.impl.dv.util.Base64.encode(data);
    	return encoded;
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
