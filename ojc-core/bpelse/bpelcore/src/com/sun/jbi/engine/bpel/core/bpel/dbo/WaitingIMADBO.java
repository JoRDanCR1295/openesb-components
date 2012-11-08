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
 * @(#)WaitingIMADBO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo;

import com.sun.jbi.engine.bpel.core.bpel.persist.DBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public interface WaitingIMADBO extends DBObject {

    /** insert statement */
    String BASE_INSERT_STMT_STR = "insert into " + PersistenceDBSchemaCreation.WAITINGIMA + " (stateid, partnerlink, operation) values (?, ?, ?)"; //$NON-NLS-1$ //$NON-NLS-1$

    /**
     * DOCUMENT ME!
     *
     * @return String engine ID
     */
    String getId();

    /**
     * DOCUMENT ME!
     *
     * @return String engine location
     */
    String getPartnerLinkName();

    /**
     * DOCUMENT ME!
     *
     * @return long engine lease expiration
     */
    String getOperationName();
}
