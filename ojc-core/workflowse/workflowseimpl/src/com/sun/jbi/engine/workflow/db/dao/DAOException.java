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
 * @(#)DAOException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.dao;

/**
 * This class is the base exception for the DAO package.
 *
 * @author SeeBeyond Technology Corporation
 * @version 
 *
 * @since eInsight5.0
 */
public class DAOException extends Exception {
    private Throwable detail;

    /**
     * DAOException Constructor
     *
     * @param str String
     */
    public DAOException(String str) {
        super(str);
    }

    /**
     * DAOException Constructor
     *
     * @param str String
     * @param t Throwable
     */
    public DAOException(String str, Throwable t) {
        super(str, t);
        detail = t;
    }

    /**
     * DAOException Constructor
     *
     * @param t Throwable
     */
    public DAOException(Throwable t) {
        super(t.getMessage(), t);
        detail = t;
    }
}
