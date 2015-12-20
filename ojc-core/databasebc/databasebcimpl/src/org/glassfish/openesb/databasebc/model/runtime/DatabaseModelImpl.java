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
 * @(#)DatabaseModelImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.model.runtime;

import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;
import java.util.logging.Level;


/**
 * @author  Venkat P
 *
 */
public class DatabaseModelImpl implements DatabaseModel {
    private static final Messages mMessages = Messages.getMessages(DatabaseModelImpl.class);
    private static final Logger mLogger = Messages.getLogger(DatabaseModelImpl.class);

    public DatabaseModelImpl(){
    }

    private static final DatabaseModelImpl instance = new DatabaseModelImpl();
    public static final DatabaseModelImpl getInstance(){
        return instance;
    }

    //@Override
    public String generateSelectQuery(final String TableName, final int rowCount) {
        return "SELECT * FROM " + TableName + " WHERE $WHERE FOR UPDATE";
    }
}
