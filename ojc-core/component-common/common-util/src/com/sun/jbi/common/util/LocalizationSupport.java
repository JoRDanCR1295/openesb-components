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
 * @(#)LocalizationSupport.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.util;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
* Tools for obtaining localized messages.  
* <p>
* Components should extend this class, following {@link I18n} as a model,
* to serve as a global i18n utility.  Each component requires at least
* one such implementation to indicate where the message bundle will reside.
* <p>
* The source for this class was copied from 
* <a href="http://hulp.dev.java.net/">http://hulp.dev.java.net/</a>.
* Inclusion in common-util simplifies the component's bootstrap classpath due to
* JBI classloading mechanisms. 
*
* @author Frank Kieviet
*/
public abstract class LocalizationSupport {
	private PropertyResourceBundle mBundle;
	private Pattern mIdPattern;
	private String mPrefix;
	private String mBundleName;
	
	/**
	 * Default name for resource bundles
	 */
	public static final String DEFAULTBUNDLENAME = "msgs";
	
	/**
	 * Default pattern to parse a message with
	 */
	public static Pattern DEFAULTPATTERN = Pattern.compile("([A-Z]\\d\\d\\d)(: )(.*)", Pattern.DOTALL); 
	
	/**
	 * Constructs a <code>LocalizationSupport</code>.
	 * @param idpattern pattern to parse message
	 * @param prefix module name
	 * @param bundlename The name of the localized bundle to generate.
	 */
	protected LocalizationSupport(Pattern idpattern, String prefix, String bundlename) {
	  mIdPattern = idpattern == null ? DEFAULTPATTERN : idpattern;
	  mPrefix = (Util.isEmpty(prefix)) ? "" : prefix +"-";
	  mBundleName = bundlename == null ? DEFAULTBUNDLENAME : bundlename;
	  // Strip off the class to obtain the package name
	  String packagename = this.getClass().getName();
	  int lastdot = packagename.lastIndexOf(".");
	  packagename = packagename.substring(0, lastdot);
	  try {
	      mBundle = (PropertyResourceBundle) ResourceBundle.getBundle(packagename + "." + mBundleName, 
	          Locale.getDefault(), getClass().getClassLoader());
	  } catch (Exception e) {
	      throw new RuntimeException("Resource bundle could not be loaded: " + e, e);
	  }
	  
//	  // Classname should be Localizer so that later the Ant task can be extended to 
//	  // automatically detect packages and resource bundles
//	  String clname = this.getClass().getName();
//	  if (!clname.endsWith(".Localizer") && !clname.endsWith("$Localizer")) {
//	      throw new RuntimeException("Localizer class [" + clname + "] should be [Localizer]");
//	  }
	}
	
	/**
	 * Constructs a <code>LocalizationSupport</code>.
     * @param prefix module name
	 */
	protected LocalizationSupport(String prefix) {
	  this(null, prefix, null);
	}
	
	private static String salvage(Object[] args) {
	  StringBuffer ret = new StringBuffer();
	  ret.append(" [FORMAT ERROR.");
	  if (args != null && args.length > 0) {
	      ret.append(" Arguments: ");
	  }
	  for (int i = 0; i < args.length; i++) {
	      if (i != 0) {
	          ret.append(", ");
	      }
	      ret.append("{").append(i).append("}=\"").append(args[i]).append("\"");
	  }
	  ret.append("]");
	  return ret.toString();
	}
	
	/**
	 * Utility method to format a message with replacement tokens.
	 * @param msg The message.
	 * @param args Optional replacement tokens.
	 * @return
	 */
	public static String format(String msg, Object... args) {
	  try {
	      return MessageFormat.format(msg, args);
	  } 
	  catch (Exception e) {
	      // Format error, e.g. "my msg{{0}/{1}", return "my msg{{0}/{1} [invalid format, {0}=xx, {1}=yy]"
	      return msg + salvage(args);
	  }
	}
	
	/**
	* Msg is a string of the form "E001: this is a message". The format of the id is 
	* specified in the constructor, and is the first group of the pattern. E.g. in a 
	* specified pattern of "([A-Z]\d\d\d)(: )( .*), the message id is [A-Z]\d\d\d; in 
	* the example that would be E001.
	* 
	* @param msg Message to be localized
	* @param args arguments The message's replacement tokens.
	* @return the localized message.
	*/
	protected String t(String msg, Object... args) {
	  try {
	      Matcher matcher = mIdPattern.matcher(msg);
	      if (!matcher.matches() || matcher.groupCount() <= 1) {
	          // Improperly formatted string: no ID.
	          // e.g. "This is a message", return "MODULE: This is a message" 
	          return (Util.isEmpty(mPrefix))
                          ? "I18N: " + format(msg, args)                //NOI18N
                          : mPrefix + ": " + format(msg, args);         //NOI18N
	      } else {
	          // Properly formatted message, e.g. "X001: This is a message"; get "X001"
	          String msgid = matcher.group(1);
	          
	          // Load string
	          String localizedmsg;
	          try {
	              localizedmsg = mBundle.getString(mPrefix + msgid);
	          } catch (Exception e) {
	              // load error, return "MODULE-E999: This is a message" (English)
	              return mPrefix + format(msg, args);
	          }
	
	          // Return localized "MODULE-X001: Dit is een bericht"
	          return mPrefix + msgid + ": "                         //NOI18N
                          + format(localizedmsg, args);
	      } 
	  } 
	  catch (Exception e) {
	      return mPrefix + format(msg, args);
	  }
	}
}
