/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)NlsMsg.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.common.utils;

/******************************************************************************
 * Copyright (c) 1996, Software Technologies Corporation, All Rights Reserved *
 *                                                                            *
 * This program, and all the routines referenced herein, are the proprietary  *
 * properties and trade secrets of SOFTWARE TECHNOLOGIES CORPORATION.         *
 *                                                                            *
 * Except as provided for by license agreement, this program shall not be     *
 * duplicated, used or disclosed without the written consent, signed by an    *
 * officer of SOFTWARE TECHNOLOGIES CORPORATION.                              *
 *                                                                            *
 ******************************************************************************/

import java.util.*;
import java.io.*;
import java.text.*;

/**
 *  <P>
 *  Handles getting strings that are displayed from
 *  a language file.
 *  <P>
 *  Hides having to worry about passing around a Properties object
 *  <p>
 * If a package wishes to have its own NlsProperty that package
 * should extend this class, override getMsg to load in
 * the proper file for that package and include their
 * own copy of _nls_prop;
 * <p>
 * <STRONG>WARNING:</STRONG>This code should NOT be used with 1.1 java
 * but instead the built in 1.1 nls functions should be used.
 *
 * @see     com.sun.stc.common.utils.CommonMsg for an example NlsMsg class
 * @author  Andrea Spilholtz
 */
public class NlsMsg extends Object
{
   /* ********************************************************* */
   /* CONSTANTS                                                 */
   /* ********************************************************* */
	  public final static String DEFAULT_FILE="egate.properties";

   /* ********************************************************* */
   /* PRIVATE DATA                                              */
   /* ********************************************************* */
	  public  static ResourceBundle _nls_prop=null;
	  public  static String _default_path="";
	  public  static boolean _montana=true;

          protected static Locale _default_locale = 
           Utils.getDefaultLocale(); 


	  public static boolean getMontana()
	  {
  	    return _montana;
	  }  
   /* ********************************************************* */
   /* PUBLIC METHODS                                            */
   /* ********************************************************* */

         public static boolean setCurrentLocale(String language)
         {
           return setCurrentLocale(language, "");
         }

         public static boolean setCurrentLocale(String language, String country)
         {
            Locale l = new Locale(language, country);
            boolean locale_supported = false;
            if (l != null)
            {
// System.out.println("set current locale: " + l);
              locale_supported = true;
              NlsMsg._default_locale = l;
            }
            return locale_supported;
         }

         public static Locale getCurrentLocale()
         {
// System.out.println("current locale: " + NlsMsg._default_locale);
           return NlsMsg._default_locale;
         }

	 /**
	  * <p>
	  * Get a "message"
	  * </p>
	  * @param     key		key associated with message
	  * @return    the message
	  */
	  public static String getMsg(String key)
	  {
// System.out.println("getMsg(key)");
		if (_nls_prop==null)
		{
		   _nls_prop=loadNlsProp(DEFAULT_FILE);
		}
                String value = null;
                try
                {
                   value = _nls_prop.getString(key);
                }
                catch (Exception e)
                {
                }
                if (value == null)
                {
                  value = key;
                }
// System.out.println("msg: " +  key + "=" +  value);
                return value;
	  }  
	 /**
	  * <p>
	  * Get a "message" and do string substitution
	  * </p>
          * <p>
          * For example if you .properties file contained the following:
          *    SPACE_SHIP_MSG=At {2,time,short} on {2,date,long}, we \
          *     detected {1,number,interger} spaceships on the planet \ 
          *     {0}.
          *    PLANET=Mars
          * And your args where:
          *    Object[] args = { 
          *       NlsMsg.getMsg("PLANET"), 
          *       new Integer(7),
          *       new Date()
          *    };
          * The message might look as follows:
          *   At 1:15 PM on April 13, 1998, we detected 7 spaceships on the
          *   planet Mars.
          * </p>
	  * @param     key	   key associated with a message that has 
          *                        formating information in it
	  * @param     argv        array of Objects to substitue for
          *                        for {argument}. See the 
          *                        MessageFormat  class for a full
          *                        description of the argument syntax 
	  * @return    the message
	  */
	  public static String getMsg(String key, Object[] argv)
	  {
// System.out.println("getMsg(key, object[]");
		return getMsg(_nls_prop,DEFAULT_FILE,key,argv);
	  }

          public static String getMsg(String key, ResourceBundle res)
          {
// System.out.println("getMsg(key, res)");
             String result = null;
             try
             {
               result = res.getString(key);
             }
             catch (Exception e)
             {
             }
             if (result == null)
             {
                result = key;
             }
// System.out.println("key:" + key  + "=" + result);
             return result;
          }
  
	  public static String getMsg(String key, ResourceBundle prop, String def_file)
	  {
		if (prop==null)
		{
		   prop=loadNlsProp(def_file);
		}
                return getMsg(key, prop);
	  }

	  public static String getMsg(ResourceBundle prop, String def_file,  
			 String key, String[] argv)
          {
            return getMsg(prop, def_file, key, (Object[]) argv);
          }
	  protected static String getMsg(ResourceBundle prop, String def_file,  
			 String key, Object[] argv)
	  {
// System.out.println("getMsg(object[])");

           MessageFormat formatter = new MessageFormat("");
           formatter.setLocale(NlsMsg._default_locale);
           String msg="";
	   if (prop==null)
	   {
	     prop=loadNlsProp(def_file);
	   }
           try
           {
             String template = prop.getString(key);
// System.out.println("template="+template);
             formatter.applyPattern(template);
             msg = formatter.format(argv);
           }
           catch (Exception e)
           {
// System.out.println("Error: " + e.getMessage());
             msg = key;
           }         
   
// System.out.println("msg: " + key + "=" +  msg);
	   return msg;
	  }
  
	  public static ResourceBundle loadNlsProp(String file)
	  {
                 String name = "msg." + 
                   Utils.getBasenameNoExt(file);
                 
		 ResourceBundle nls_prop=null;
                 try
                 {
// System.out.println("loading resource bundle " + name);
// System.out.println("locale = " + getCurrentLocale().toString());
		   nls_prop=ResourceBundle.getBundle(name,
                     NlsMsg._default_locale);
                 }
                 catch (MissingResourceException e)
                 {
                   System.out.println(e.getMessage());
                   e.printStackTrace();
                 }
		 return nls_prop;
	  }

	  public static ResourceBundle loadNlsProp(
             String file, 
             String lang)
          {
             return loadNlsProp(file, lang, "");
          }
  
	  public static ResourceBundle loadNlsProp(
             String file, 
             String lang,
             String country)
	  {
                 setCurrentLocale(lang, country);
		 return loadNlsProp(file);
	  }





   /* ********************************************************* */
   /* UNIT TEST                                                 */
   /* ********************************************************* */
   public static void main(String[] args)
   {
	  _nls_prop=NlsMsg.loadNlsProp("try.nls");
	  System.out.println(NlsMsg.getMsg("CAN_NOT_OPEN_FILE"));
	  System.out.println(NlsMsg.getMsg("CAN_NOT_READ_FILE"));
   }   
	  /**
	   * Set the path where the nls message files are expected to be.
	   */
	  public static void setDefaultPath(String path)
	  {
		File fpath = new File(path);
		if (fpath.isDirectory() && fpath.canRead())
		 _default_path=path;
	  }  
	  public static void setMontana(boolean mode)
	  {
	_montana = mode;
	  }

          private static File getLocaleFile(String file_path)
          {
            File file = new File(file_path);
            String dir  = Utils.getDirname(file_path);
            String fn   = Utils.getBasenameNoExt(file_path);
            String ext  = Utils.getExtension(file_path);
            String locale_fn = dir + File.separator + 
             fn + "_" + Utils.getLocaleString() + "." + ext;
            File locale_file = new File(locale_fn);
            if (locale_file.exists())
            {
              file = locale_file;
            } 
// System.out.println("Loading...." + file.getAbsolutePath());
            return file;
          }
  
	  private static void showErrorDialog(String msg)
	  {
		int option;
 
	javax.swing.JFrame frame = new javax.swing.JFrame();
		option = javax.swing.JOptionPane.showConfirmDialog(frame,
											   msg,
											   "Error",
											   javax.swing.JOptionPane.DEFAULT_OPTION,
											   javax.swing.JOptionPane.ERROR_MESSAGE);
		if (option == javax.swing.JOptionPane.OK_OPTION)
		{
			 System.exit(1);
		}
	  }  
}
