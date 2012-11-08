/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author liyunhai
 */
public class PagedResultInfo {

    private static Map mCookie = new HashMap();

    private PagedResultInfo() {
    }

    public static byte[] getCookie(String key) {
        return (byte[]) mCookie.get(key);
    }

    public static void setCookie(String key, byte[] cookie) {
        mCookie.put(key, cookie);
    }
    
    public static void removeCookie(String key) {
        if (mCookie.containsKey(key)) {
            mCookie.remove(key);
        }
    }
}
