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

package com.sun.jbi.engine.iep.core.runtime.debugger;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author rdwivedi
 */
public class DebugInfo {
    String mOprName = null;
    Map<String,Object> debugInfo = null;
    public DebugInfo(String oprName) {
        mOprName = oprName;
        debugInfo = new HashMap<String,Object>();
    }
    public void addDebugInfo(String key, Object value){
        debugInfo.put(key, value);
    }
    public Map getDebugInfo() {
        return debugInfo;
    }
    
    public String getString() {
        StringBuffer buffer  = new StringBuffer() ;
        Set set = debugInfo.keySet();
        if(set!= null){
            Iterator iter = set.iterator();
            while(iter.hasNext()){
                String k = (String) iter.next();
                buffer.append(k+"\t");
                Object t = debugInfo.get(k);
                if(t!= null){
                    buffer.append(debugInfo.get(k).toString()+"\t");
                } else {
                    buffer.append("NULL"+"\t");
                }
                buffer.append("\n");
            }
        }
        
        return buffer.toString();
        
    }

}
