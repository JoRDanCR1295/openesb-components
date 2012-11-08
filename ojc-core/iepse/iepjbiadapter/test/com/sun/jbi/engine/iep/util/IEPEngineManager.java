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

package com.sun.jbi.engine.iep.util;

import com.sun.jbi.engine.iep.core.runtime.DefaultIEPEngine;
import com.sun.jbi.engine.iep.core.runtime.IEPEngine;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlanInfo;
import com.sun.jbi.engine.iep.core.runtime.util.NameUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.util.Properties;
import java.io.File;
import java.util.ArrayList;

/**
 *
 * @author rdwivedi
 */
public class IEPEngineManager {
    
    private IEPEngine mEngine = null;
    private Properties mConfigProps = null;
    
    
    public void startIEPEngineWithEmbeddedDerby() throws Exception {
        DefaultIEPEngine engine = DefaultIEPEngine.getInstance();
        Properties props = new Properties();
        props.setProperty("RuntimeStyle", "embedded");
        props.setProperty("DatabaseSchemaName", "APP");
        props.setProperty("IepDerbyJarPath", "lib\\iepsederby.jar");
        engine.init(props);
        mEngine = engine;
        mConfigProps = props;
    }
    
    public void deployIEPProcessToEngine(File iepFile ) throws Exception {
        // see com.sun.jbi.ui.devtool.projects.ieppro.anttasks.iep.GenerateAsaArtifacts
        //String instanceId = NameUtil.makeJavaId(fPath.substring(serviceUnitRootPathLen));
        String instanceId = NameUtil.makeJavaId(iepFile.getName());
        QueryPlanInfo qpi = new QueryPlanInfo(instanceId, iepFile.getPath());
        ArrayList<QueryPlanInfo> list = new ArrayList<QueryPlanInfo>();
        list.add(qpi);
        mEngine.deploy(list);
    }
    
    public IEPEngine getIEPEngine() {
        return mEngine;
    }
    
    public QueryPlan getPlan(String instanceId) {
       QueryPlan plan =  Util.getPlanByInstanceId(mConfigProps, instanceId);
       return plan;
    }

}
