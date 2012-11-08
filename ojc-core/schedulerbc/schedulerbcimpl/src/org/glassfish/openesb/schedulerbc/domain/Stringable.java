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
 * @(#)Stringable.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain;

/**
 * Interface describing objects than can be converted to/from String.
 * <p>
 * Objects are represented as where braces are not part of the data but
 * right-parenthesis is:
 * <pre>
 * {className}){length-1}){string-1}{length-2}){string-2}...
 * </pre>
 *
 * @author sunsoabi_edwong
 * @version 2.1
 */
public interface Stringable {

    public static final String DELIM = ")";                             //NOI18N

    /**
     * Reads from a string to populate the <code>Stringable</code> object.
     *
     * @param data String data representing the <code>Stringable</code> object.
     * @throws org.glassfish.openesb.schedulerbc.domain.StringableException
     */
    void readObject(String data) throws StringableException;

    /**
     * Writes the <code>Stringable</code> object to a string buffer.
     * 
     * @param data Buffer to write the string representation of the
     *        <code>Stringable</code> object.
     * @throws org.glassfish.openesb.schedulerbc.domain.StringableException
     */
    void writeObject(StringBuilder data) throws StringableException;

    /**
     * Gets the string representation of the <code>Stringable</code> object.
     *
     * @return
     * @throws org.glassfish.openesb.schedulerbc.domain.StringableException
     */
    String valueAsString() throws StringableException;
}
