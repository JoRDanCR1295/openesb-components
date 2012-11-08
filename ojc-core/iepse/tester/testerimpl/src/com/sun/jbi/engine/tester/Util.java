/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.tester;

import java.io.File;
import java.io.FileInputStream;
import java.util.Properties;

/**
 *
 * @author radval
 */
public class Util {

    public static Properties readConfiguration(String configFileLocation) {
         Properties prop = new Properties();
         try {
             
             File fin = new File(configFileLocation);
             
             FileInputStream fis = new FileInputStream(fin);
             prop.load(fis);
             
             fis.close();
         } catch(Exception ex) {
             ex.printStackTrace();
         } finally {
        	 
        	 
         }
         
         return prop;
    }
    
    public static Properties readConfiguration(File configFileLocation) {
         Properties prop = new Properties();
         try {
             
             FileInputStream fis = new FileInputStream(configFileLocation);
             prop.load(fis);
             fis.close();
         } catch(Exception ex) {
             ex.printStackTrace();
         }
         
         return prop;
    }
}
