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
 * @(#)CommonMsg.java 
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

/**
 *  <P>
 *  Handles getting strings that are displayed from the Common
 *  language file.
 * @author  Andrea Spilholtz
 */ 
public class CommonMsg extends NlsMsg
{
   /* ********************************************************* */
   /* CONSTANTS                                                 */
   /* ********************************************************* */
	  public static String DEFAULT_FILE="common.properties";

   /* ********************************************************* */
   /* PRIVATE STATIC DATA                                       */
   /* ********************************************************* */
	  private static ResourceBundle _common_nls_prop=loadNlsProp(DEFAULT_FILE);


/* ********************************************************* */
/* PUBLIC METHODS                                            */
/* ********************************************************* */

/**
  * <p>
  * Get a "message"
  * </p>
  * @param     key		key associated with message
  * @return    the message
  */
public static String getMsg(String key)
{
	return getMsg(key, _common_nls_prop);
}
	 /**
	  * <p>
	  * Get a "message" expanding all {arg} with their arguments
	  * </p>
	  * <p>
	  * @param     key	    key associated with message
	  * @param     argv         an array of objects that will be 
	  *                         substituted in the resource bundle
	  * @return    the message
	  */
	  public static String getMsg(String key, Object[] argv)
	  {
		return getMsg(_common_nls_prop,DEFAULT_FILE,
			  key,argv);
	  }

	 /**
	  * <p>
	  * Get a "message" with right blank padding.  In the respective
          * .properties file, there MUST be another key entry with the
          * same name with a "_PAD_LEN" suffix.  This value should be the
          * EXTRA padding characters for the message; NOT the total
          * desired width.
	  * </p>
	  * <p>
	  * @param     key	    key associated with message
	  * @return    the padded message
	  */
          public static String getPaddedMsg(String key)
          {
            String value = getMsg(key);
            String len_string = getMsg(key + "_PAD_LEN");
            String padded_string = null;
            int len = 0;
            if (value != null && len_string != null)
            {
              try
              {
                len = Integer.parseInt(len_string) + value.length();
                padded_string = Utils.padString(value, len);
              }
              catch (Exception e)
              {
              }
            }
            return padded_string;
          }

	 /**
	  * <p>
	  * Get a "message" with centered blank padding.  In the respective
          * .properties file, there MUST be another key entry with the
          * same name with a "_PAD_LEN" suffix.  This value should be the
          * EXTRA padding characters for the message; NOT the total
          * desired width.
	  * </p>
	  * <p>
	  * @param     key	    key associated with message
	  * @return    the centered message
	  */
          public static String getCenteredMsg(String key)
          {
            String value = getMsg(key);
            String len_string = getMsg(key + "_PAD_LEN");
            String padded_string = null;
            int len = 0;
            if (value != null && len_string != null)
            {
              try
              {
                len = Integer.parseInt(len_string) + value.length();
                padded_string = Utils.centerString(value, len);
              }
              catch (Exception e)
              {
              }
            }
            return padded_string;
          }

      
   /* ********************************************************* */
   /* UNIT TEST                                                 */
   /* ********************************************************* */
   public static void main(String[] args)
   {
		System.out.println(CommonMsg.getMsg("CAN_NOT_UPDATE_VALUE"));
		String[] eargs=new String[2];
		eargs[0]="a";
		eargs[1]="b";
		System.out.println(CommonMsg.getMsg("EXAMPLE_SUB",eargs));
   }   
	  public static void setDefault(String file)
	  {
		DEFAULT_FILE=file;
	  }  
}
