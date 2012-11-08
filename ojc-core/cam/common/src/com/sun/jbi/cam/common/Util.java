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
 * @(#)Util.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.common;

/**
 *
 * @author ylee
 */
public class Util {
    
    /** Creates a new instance of Util */
    public Util() {
    }
 
    
    public static String replaceInvalidChars(String value) {
        return value.replaceAll("\\.","_");
    }
    
    public static String fixupName(String name, String prefix) {
        if ( name.startsWith(prefix) ) {
            return name.substring(prefix.length()+1);
        } else {
            return name;
        }
    }
    
    public static String mapType(String type) {
        String rtype = type;
        if ( GenericConstants.BC_TYPE.equals(type) ) {
            rtype = GenericConstants.BC_TYPE_MBEAN_NAME;
        } else if ( GenericConstants.SE_TYPE.equals(type) ) {
            rtype = GenericConstants.SE_TYPE_MBEAN_NAME;
        } else if ( GenericConstants.SU_TYPE.equals(type) ) {
            rtype = GenericConstants.SU_TYPE_MBEAN_NAME;
        }
        return rtype;
    }
    
    
    public static String mapInstalledType(String type) {
        String rtype = type;
        if ( GenericConstants.SE_TYPE.equals(type) ) {
            rtype = GenericConstants.ENGINE_INSTALLED_TYPE;
        } else if ( GenericConstants.BC_TYPE.equals(type) )  {
            rtype = GenericConstants.BINDING_INSTALLED_TYPE;
        }
        return rtype;
    }
    
    
    public static String convertType(Object type) {
        String ctype = "";
        if ( type instanceof Boolean ) {
            ctype = type.toString();
        } else {
            ctype = type + "";
        }
        return ctype;
    }
    
    /**
     * map component values -
     *  if ( container==# ) then return the component value
     */
    public static String mapComponentValue(String containerValue, String componentValue) {
        if ( GenericConstants.HASH_SEPARATOR.equals(containerValue) ) {
            return componentValue;
        }
        return containerValue;
    }
    
    
    public static String trimRight(String str, String token) {
        String result="";
        if (str!=null ) {
            result = str.substring(0,str.lastIndexOf(token));
        }
        return result;
    }
    
    
    public static String trimLeft(String str,String token) {
        String result="";
        if ( str!=null ) {
            result = str.substring(str.indexOf(token)+1);
        }
        return result;
    } 
    
    
    public static String getNamespace(String endpoint,String token) {
        String result="";
        if ( endpoint!=null ) {
            result = endpoint.substring(0,endpoint.indexOf(token));
        }
        return result;
    }
    
}
