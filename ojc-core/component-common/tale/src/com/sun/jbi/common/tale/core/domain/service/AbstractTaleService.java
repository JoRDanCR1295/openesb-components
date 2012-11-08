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
 * @(#)AbstractTaleService.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.domain.service;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.common.tale.core.domain.TaleDomain;
import com.sun.jbi.common.tale.core.persist.dao.DAO;
import com.sun.jbi.common.tale.core.persist.dao.DAOException;
import com.sun.jbi.common.tale.core.util.I18n;
import com.sun.jbi.common.tale.core.util.TaleException;

/**
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractTaleService implements TaleService {
    private TaleDomain mDomain;
    private Logger mLogger;
    
    protected AbstractTaleService(TaleDomain domain) {
        mDomain = domain;
        //mLogger = domain.getDomainLogger(this.getClass().getName());
        mLogger = Logger.getLogger(this.getClass().getName());
    }

    /**
     * @return the domain
     */
    public TaleDomain getDomain() {
        return mDomain;
    }

    protected Logger log() {
        return mLogger;
    }
    
    protected void release(DAO dao) {
        try {
            if (dao != null) dao.release();
        }
        catch (DAOException de) {
            log().log(Level.WARNING, 
                      I18n.loc("TALE-6021: Failed to release DAO: {0}", de.getMessage()), 
                      de);
        }
    }
    
    protected TaleException error(Exception cause, String msg) {
        if (cause == null) {
            log().warning(msg);
            return new TaleException(msg);
        }
        else {
            log().log(Level.WARNING, msg, cause);
            return new TaleException(msg, cause);
        }
    }
}
