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
 * @(#)AspectServiceUnitsBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.aspects.manager.framework.generic;

import com.sun.data.provider.TableDataProvider;
import com.sun.data.provider.impl.ObjectListDataProvider;
import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.Util;
import com.sun.jbi.cam.manager.framework.common.BaseBean;
import com.sun.jbi.cam.manager.framework.generic.DisplayServiceUnits;
import com.sun.jbi.cam.manager.framework.generic.ServiceUnitsBean;
import com.sun.jbi.cam.model.management.JBIServiceAssemblyStatus;
import com.sun.jbi.cam.model.management.JBIServiceUnitStatus;
import com.sun.jbi.cam.plugins.aspects.common.AspectsGenericConstants;
import com.sun.jbi.cam.services.administration.AdministrationService;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

/**
 *
 * @author graj
 */
public class AspectServiceUnitsBean extends ServiceUnitsBean implements Serializable {
	private static final long serialVersionUID = 1L;  
    private transient Logger logger = Logger.getLogger(AspectServiceUnitsBean.class.getName());
    
    /**
     * Creates a new instance of AspectServiceUnitsBean
     */
    public AspectServiceUnitsBean() {
        setup();
    }
    
    @Override
    public List<DisplayServiceUnits> getServiceUnitList(String serviceAssemblyName) {
        // setup request configuration data
        setup();
        
        List<DisplayServiceUnits> list = new ArrayList<DisplayServiceUnits>();
        
        List<JBIServiceAssemblyStatus> saList = getSAList(tName);
        Iterator<JBIServiceAssemblyStatus> saIter = saList.iterator();
        while( saIter.hasNext()) {
            JBIServiceAssemblyStatus saStatus = saIter.next();
            String saName = saStatus.getServiceAssemblyName();
            Iterator<JBIServiceUnitStatus> suIter = saStatus.getJbiServiceUnitStatusList().iterator();
            while( suIter.hasNext() ) {
                JBIServiceUnitStatus suStatus = suIter.next();
                String target = suStatus.getTargetName();
                // add DisplayServiceUnits on component match
                // or if the component is service assembly that matches
                // the service assembly provided has parameter to this method
                if ( target.equals(componentName) ||
                        (componentName.equals(serviceAssemblyName) &&
                        componentType.equals(GenericConstants.SA_TYPE))) {
                    String name = suStatus.getServiceUnitName();
                    String desc = suStatus.getServiceUnitDescription();
                    String providerUrl = getProviderUrl(
                            Util.fixupName(name,saName),
                            GenericConstants.SU_TYPE,
                            componentName,
                            componentType);
                    String url = providerUrl+"?"
                            +GenericConstants.COMPONENT_NAME+"="+Util.fixupName(name,saName)
                            +"&"+GenericConstants.COMPONENT_TYPE+"="+GenericConstants.SU_TYPE
                            +"&"+GenericConstants.COMPONENT_CNAME+"="+componentName
                            +"&"+GenericConstants.COMPONENT_CTYPE+"="+componentType  //;
                            +"&"+GenericConstants.COMPONENT_PNAME+"="+pName
                            +"&"+GenericConstants.COMPONENT_TNAME+"="+tName
                            +"&"+AspectsGenericConstants.SERVICE_UNIT_NAME_KEY+"="+name;
                    System.out.println("******* Provider URL is:"+providerUrl);
                    System.out.println("******* URL is:"+url);
                    String status = suStatus.getStatus();
                    DisplayServiceUnits su = new DisplayServiceUnits(name,desc,url,status);
                    list.add(su);
                }
            }
        }
        return list;
    }
    
    
    
}
