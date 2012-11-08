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
 * @(#)PropertyUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.util;

import com.sun.jbi.engine.iep.core.share.SharedConstants;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.ParseException;
import java.util.Date;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;

/**
 * PropertyUtil.java
 *
 * Created on May 25, 2005, 11:05 AM
 *
 * @author Bing Lu
 */
public class PropertyUtil {
    private static final Messages mMessages = Messages.getMessages(PropertyUtil.class);
    
    public static boolean getboolean(Map prop, String propName, boolean defaultValue) {
        if (prop == null) {
            return defaultValue;
        }
        Object temp = prop.get(propName);
        if (temp == null) {
            if (!(prop instanceof Properties)) {
                return defaultValue;
            }
            temp = ((Properties)prop).getProperty(propName);
            if (temp == null) {
                 return defaultValue;
            }
        }
        if (temp instanceof Boolean) {
            return ((Boolean)temp).booleanValue();
        }
        boolean ret = Boolean.parseBoolean(temp.toString());
        return ret;
    }
 
    public static int getint(Map prop, String propName, int defaultValue) {
        if (prop == null) {
            return defaultValue;
        }
        Object temp = prop.get(propName);
        if (temp == null) {
            if (!(prop instanceof Properties)) {
                return defaultValue;
            }
            temp = ((Properties)prop).getProperty(propName);
            if (temp == null) {
                 return defaultValue;
            }
        }
        if (temp instanceof Integer) {
            return ((Integer)temp).intValue();
        }
        try {
            int ret = Integer.parseInt(temp.toString());
            return ret;
        } catch (NumberFormatException e) {
        }
        return defaultValue;
    }
    
    public static long getDateStringAsLong(Map prop, String propName, long defaultValue) {
        if (prop == null) {
            return defaultValue;
        }
        String temp = (String)prop.get(propName);
        if (temp == null) {
            if (!(prop instanceof Properties)) {
                return defaultValue;
            }
            temp = ((Properties)prop).getProperty(propName);
            if (temp == null) {
                 return defaultValue;
            }
        }
        try {
            Date date = SharedConstants.DATE_FORMAT.parse(temp);
            long ret = date.getTime();
            return ret;
        } catch (ParseException e) {
        }
        return defaultValue;
    }
    
    public static long getlong(Map prop, String propName, long defaultValue) {
        if (prop == null) {
            return defaultValue;
        }
        Object temp = prop.get(propName);
        if (temp == null) {
            if (!(prop instanceof Properties)) {
                return defaultValue;
            }
            temp = ((Properties)prop).getProperty(propName);
            if (temp == null) {
                 return defaultValue;
            }
        }
        if (temp instanceof Long) {
            return ((Long)temp).intValue();
        }
        try {
            long ret = Long.parseLong(temp.toString());
            return ret;
        } catch (NumberFormatException e) {
        }
        return defaultValue;
    }
    
    public static double getdouble(Map prop, String propName, double defaultValue) {
        if (prop == null) {
            return defaultValue;
        }
        Object temp = prop.get(propName);
        if (temp == null) {
            if (!(prop instanceof Properties)) {
                return defaultValue;
            }
            temp = ((Properties)prop).getProperty(propName);
            if (temp == null) {
                 return defaultValue;
            }
        }
        if (temp instanceof Double) {
            return ((Double)temp).doubleValue();
        }
        try {
            double ret = Double.parseDouble(temp.toString());
            return ret;
        } catch (NumberFormatException e) {
        }
        return defaultValue;
    }
    
    public static String getString(Map prop, String propName, String defaultValue) {
        if (prop == null) {
            return defaultValue;
        }
        Object temp = prop.get(propName);
        if (temp == null) {
            if (!(prop instanceof Properties)) {
                return defaultValue;
            }
            temp = ((Properties)prop).getProperty(propName);
            if (temp == null) {
                 return defaultValue;
            }
        }
        if (temp instanceof String) {
            return (String)temp;
        }
        return temp.toString();
    }
    
    public static Properties getPropertiesFromString(String s, String equalSign, String propDeliminator) {
        Properties prop = new Properties();
        StringTokenizer st = new StringTokenizer(s, propDeliminator);
        while (st.hasMoreTokens()) {
            String pair = st.nextToken();
            int i = pair.indexOf(equalSign);
            if (i < 0) {
                prop.put(pair, "");
            } else {
                String name = pair.substring(0, i);
                String value = pair.substring(i + equalSign.length());
                prop.put(name, value);
            }
        }
        return prop;
    }
    
    public static long getMiliseconds(double time, String unit) throws IllegalArgumentException{
        if (unit.equals(SharedConstants.TIME_UNIT_SECOND)) {
            return Math.round(time * 1000);
        } else if (unit.equals(SharedConstants.TIME_UNIT_MINUTE)) {
            return Math.round(time * 1000 * 60);
        } else if (unit.equals(SharedConstants.TIME_UNIT_HOUR)) {
            return Math.round(time * 1000 * 3600);
        } else if (unit.equals(SharedConstants.TIME_UNIT_DAY)) {
            return Math.round(time * 1000 * 3600 * 24);
        } else if (unit.equals(SharedConstants.TIME_UNIT_WEEK)) {
            return Math.round(time * 1000 * 3600 * 24 * 7);
        }
        throw new IllegalArgumentException(mMessages.getString("PropertyUtil.Undefined_time_unit", unit));
    }
    
    public static long getSeconds(double time, String unit) throws IllegalArgumentException{
        if (unit.equals(SharedConstants.TIME_UNIT_SECOND)) {
            return Math.round(time);
        } else if (unit.equals(SharedConstants.TIME_UNIT_MINUTE)) {
            return Math.round(time * 60);
        } else if (unit.equals(SharedConstants.TIME_UNIT_HOUR)) {
            return Math.round(time * 3600);
        } else if (unit.equals(SharedConstants.TIME_UNIT_DAY)) {
            return Math.round(time * 3600 * 24);
        } else if (unit.equals(SharedConstants.TIME_UNIT_WEEK)) {
            return Math.round(time * 3600 * 24 * 7);
        }
        throw new IllegalArgumentException(mMessages.getString("PropertyUtil.Undefined_time_unit", unit));
    } 
    
    public static boolean store(Properties prop, File file) {
        FileOutputStream fos = null;
        try {
            fos = new FileOutputStream(file);
            prop.store(fos, "iepse properties");
        } catch (FileNotFoundException ex) {
            return false;
        } catch (IOException ex) {
            return false;
        } finally {
            if (fos != null) {
                try {
                    fos.close();
                } catch (Exception e) {
                }
            }
        }
        return true;
    }
    
    public static boolean load(Properties prop, File file) {
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(file);
            prop.load(fis);
        } catch (FileNotFoundException ex) {
            return false;
        } catch (IOException ex) {
            return false;
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (Exception e) {
                }
            }
        }
        return true;
    }
}
