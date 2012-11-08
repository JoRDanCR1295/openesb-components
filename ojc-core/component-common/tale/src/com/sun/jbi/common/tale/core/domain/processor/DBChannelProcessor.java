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
 * @(#)DBChannelProcessor.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.domain.processor;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.common.tale.core.persist.dao.JpaDAO;
import com.sun.jbi.common.tale.core.domain.TaleRequest;
import com.sun.jbi.common.tale.core.util.TaleException;
import com.sun.jbi.common.tale.core.util.I18n;

/**
 * Implements the Database Channel for ALE.
 * @author Sun MicroSystems
 */
public class DBChannelProcessor implements Processor {
    
    private Logger mLogger = Logger.getLogger(this.getClass().getName());

    /** @see com.sun.jbi.engine.ale.core.domain.processor.Processor#execute(com.sun.jbi.engine.ale.core.domain.ALERequest) */
    public void execute(TaleRequest request) throws TaleException {
        JpaDAO dao = null;
        try {
            dao = new JpaDAO();
            dao.initEntityManager();
            dao.persistRecordsInDB(request);
        } catch (Exception e) {
            //To Do: Put it to dead letter queue
            String i18n = I18n.loc("ALECORE-7019: Failed to execute DBChannelProcessor: {0}", e.getMessage());
            mLogger.log(Level.SEVERE, i18n, e);
            throw new TaleException(i18n, e);
        } finally {
            dao.closeEntityManager();;
        }
    }

}
