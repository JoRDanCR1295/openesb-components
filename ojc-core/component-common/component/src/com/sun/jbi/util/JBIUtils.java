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
 * @(#)JBIUtils.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.util;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import com.sun.jbi.management.message.JBIMessageException;

/**
 * This  class contains a collection of utility methods used by the
 * various classes implementing elements in the XML JBI message schema.
 *
 * @author Sun Microsystems
 *
 */
public class JBIUtils {
    /**
     * Create a <code>List</code> that enforces class safety and optionally
     * prevents null and/or duplicate elements. This method instantiates an
     * <code>ArrayList</code>.
     *
     * @param clazz The class to be used in type-checking.
     * @param nullsAllowed Whether this list should allow null element insertion.
     * @param dupsAllowed Whether this list should allow duplicate element insertion.
     */
    public static List createSafeList(final Class clazz, final boolean nullsAllowed, final boolean dupsAllowed) {
        return new ArrayList() {
            public boolean add(Object element) {
                if (element == null && !nullsAllowed) {
                    throw new IllegalArgumentException("Cannot add null element to list");
                }

                if (element != null && !clazz.isAssignableFrom(element.getClass())) {
                    throw new ClassCastException("Incompatible types. Got: " + element.getClass().getName() + ", expected: " + clazz.getName());
                }

                if (!dupsAllowed && super.contains(element)) {
                    throw new IllegalArgumentException("Element already in list: " + element);
                }

                return super.add(element);
            }
        };
    }

    /**
     * Validate that the given collection contains only elements that are
     * type-compatible with the given class.
     *
     * @param collection The collection to be validated.
     * @param clazz The class to be used in type-checking.
     * @throws IllegalArgumentException if the collection is null or contains at
     *                                   least one non-compliant element.
     */
    public static void validateCollection(Collection collection, Class clazz) throws IllegalArgumentException {
        if (collection == null) {
            throw new JBIMessageException("List cannot be null");
        }
        Iterator i = collection.iterator();
        while (i.hasNext()) {
            Object o = i.next();
            if (o == null) {
                throw new IllegalArgumentException("Element cannot be null");
            }
            if (!clazz.isAssignableFrom(o.getClass())) {
                throw new JBIMessageException("List can only contain elements of class " + clazz.getName());
            }
        }
    }

    // TODO Method escape(String):String for escaping text as XML deserves a better place
    /**
     * Escape a string so that XML-reserved characters are mapped to their
     * entity representation. This method maps <code>&lt;</code>,
     * <code>&gt;</code>, and <code>&amp;</code>.
     * This is suitable for text elements, but might not be sufficient for
     * attributes (does not map &quot; and &apos;)
     */
    public static String escape(String text) {
        if (text == null) {
            return "";
        }

        StringBuffer buffer = new StringBuffer();
        char[] chars = text.toCharArray();
        for (int i = 0; i < chars.length; i++) {
            switch (chars[i]) {
                case '<':
                    buffer.append("&lt;");
                    break;
                case '>':
                    buffer.append("&gt;");
                    break;
                case '&':
                    buffer.append("&amp;");
                    break;
                default:
                    buffer.append(chars[i]);
            }
        }

        return buffer.toString();
    }

    /**
     * Dump a stack trace as a string.
     *
     * @param throwable The exception whose stack trace is to be printed.
     * @return The stack trace string.
     */
    public static String stringStackTrace(Throwable throwable) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        throwable.printStackTrace(pw);
        return sw.toString();
    }
}
