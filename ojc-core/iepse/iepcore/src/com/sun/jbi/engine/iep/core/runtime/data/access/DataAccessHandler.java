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

package com.sun.jbi.engine.iep.core.runtime.data.access;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;

/**
 *
 * @author rdwivedi
 */
public class DataAccessHandler {
    private OperatorDA mDA = null;
    private static final Messages mMessages = Messages.getMessages(DataAccessHandler.class);
    private Map<String,CustomExtDBTable> mCustoms ;
    public DataAccessHandler() {
        mCustoms = new HashMap<String,CustomExtDBTable>();
    }
    
    public void handle(Operator opr, Connection con){
        if(opr.isDataAccessEnabled()){
            if(mDA== null){
                mDA = new OperatorDA();
            }
            try {
                mDA.execute(opr, con);
                CustomExtDBTable extTable = mCustoms.get(opr.getName());
                if(extTable== null){
                    extTable = new CustomExtDBTable();
                    extTable.init(opr, con);
                    mCustoms.put(opr.getName(), extTable);
                }
               extTable.execute(opr, con, mDA.getTabularData()); 
            } catch(Exception e){
                // Just handle the exception and log it we don;t need the IEP process to 
                // get affected by this exception.
                mMessages.log(Level.SEVERE, "DataAccessHandler.execution_failed", new Object[]{opr.getName()}, e); 
            }
        }
    }
}
