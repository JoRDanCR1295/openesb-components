/*
 * Base64.java
 * 
 * Created on Oct 8, 2007, 4:40:01 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.databasebc.security;

/**
 *
 * @author narayan
 */
public interface Base64 {
    public String encode(String data);
    public String decode(String data);
}
