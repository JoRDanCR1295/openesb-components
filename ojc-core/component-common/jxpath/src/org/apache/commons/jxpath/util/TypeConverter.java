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
 * @(#)TypeConverter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.util;

/**
 * A type converter can be installed on TypeUtils to introduce
 * additional type conversions for JXPath. Most of
 * the time BasicTypeConverter should be used as the superclass.
 *
 * @see TypeUtils#setTypeConverter
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public interface TypeConverter {

    /**
     * Returns true if it can convert the supplied
     * object to the specified class.
     */
    boolean canConvert(Object object, Class toType);

    /**
     * Converts the supplied object to the specified
     * type. Throws a runtime exception if the conversion is
     * not possible.
     */
    Object convert(Object object, Class toType);
}