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
 * @(#)TypeUtils.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.util;

/**
 * Global type conversion utilities.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class TypeUtils {
    private static TypeConverter typeConverter = new BasicTypeConverter();

    /**
     * Install an alternative type converter.
     */
    public static synchronized void setTypeConverter(TypeConverter converter) {
        typeConverter = converter;
    }

    /**
     * Returns the current type converter.
     */
    public static TypeConverter getTypeConverter() {
        return typeConverter;
    }

    /**
     * Returns true if the global converter can convert the supplied
     * object to the specified type.
     */
    public static boolean canConvert(Object object, Class toType) {
        return typeConverter.canConvert(object, toType);
    }

    /**
     * Converts the supplied object to the specified type. May
     * throw a RuntimeException.
     */
    public static Object convert(Object object, Class toType) {
        return typeConverter.convert(object, toType);
    }
}
