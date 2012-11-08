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
 * @(#)SNMPEngineFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpengine;

import com.sun.jbi.snmpengine.impl.SNMPRAImpl;

import javax.xml.transform.Source;

/**
 * Factory for SNMP Engines
 * 
 * @author fkieviet
 */
public class SNMPEngineFactory {
    
    private class Instantiator extends SNMPRA {
        private SNMPRAImpl mImpl;
        

        @Override
        public void start(SNMPCallback listener) throws Exception {
            if (mImpl != null) {
                throw new Exception("Can call start only once");
            }
            
            SNMPRAConfig config = (SNMPRAConfig) this.clone();
            mImpl = new SNMPRAImpl(config, listener);
            mImpl.start();
        }

        @Override
        public void stop() {
            if (mImpl != null) {
                mImpl.stop();
            }
            mImpl = null;
        }

        @Override
        public void replyMetadata(String queryId, Source results) {
            if (mImpl != null) {
                mImpl.replyMetadata(queryId, results);
            }
        }

        @Override
        public void replyTrap(String batchId, boolean error) {
            if (mImpl != null) {
                mImpl.replyTraps(batchId, error);
            }
        }
        
        @Override
        public void requestPM(String msgExchangeId, Source request) {
            if (mImpl != null) {
                mImpl.requestPM(msgExchangeId, request);
            }
        }
    }
    
    /**
     * @return new SNMPRA
     */
    public SNMPRA create() {
        return new Instantiator();
    }
}
