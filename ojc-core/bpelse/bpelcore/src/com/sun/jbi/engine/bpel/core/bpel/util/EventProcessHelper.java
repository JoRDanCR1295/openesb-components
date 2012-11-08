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
 * @(#)$Id: EventProcessHelper.java,v 1.7 2009/04/07 00:30:41 vinayram Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.util;


import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;
import javax.sql.DataSource;
import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.Channel;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPELProcessManagerImpl;

public class EventProcessHelper {
    private static Logger LOGGER = Logger.getLogger(EventProcessHelper.class.getName());

    private Engine mEngine;

    private DBConnectionFactory mdbFactory;

    
    private Channel mChannel;
    
    public EventProcessHelper(Engine engine, Channel channel) {
        mEngine = engine;
        mChannel = channel;
    }    

	public void setDBFactory (DBConnectionFactory factory) {
        mdbFactory = factory;
	}

//	public DataSource getDataSource() {
//		return mDataSource;
//	}
    
    public DBConnectionFactory getFactory () {
        return mdbFactory;
    }

	public Engine getEngine() {
		return mEngine;
	}


    public void sendKPIMEx(QName interfaceName, String operation, String doc){
        try {
            mChannel.sendKPIMEx(interfaceName, operation, doc);
        } catch (MessagingException e) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6166: Encountered error {0} while sending a message to KPI EP", e), e);
        }
    }
}
