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
 * @(#)SwareSchemaException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema;

/**
 * This exception will be thrown if during reading a schema, a schema
 * invalidity or illegal state is detected.  Usually this should not happen
 * (it is assumed that all schemas being wrapped are valid ones.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public class SwareSchemaException extends Exception {

    private static final long serialVersionUID = 4920032139582789031L;

    /**
     * Constructs from a message.
     * 
     * @param message an error message
     */
    public SwareSchemaException(String message) {
        super(message);
    }

    /**
     * Constructs from a throwable.
     * 
     * @param e a throwable
     */
    public SwareSchemaException(Throwable e) {
        super(e);
    }

    /**
     * Constructs from a message and a throwable.
     * 
     * @param message an error message
     * @param e a throwable
     */
    public SwareSchemaException(String message, Throwable e) {
        super(message, e);
    }
}
