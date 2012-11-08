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
 * @(#)ControlBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.generic;

import com.sun.data.provider.TableDataProvider;
import com.sun.data.provider.impl.ObjectListDataProvider;
import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.manager.framework.common.BaseBean;
import com.sun.jbi.cam.services.management.ManagementService;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;


/**
 *
 * @author ylee
 */
public class ControlBean extends BaseBean implements Serializable {
    
    public static String BEAN_NAME = "#{ControlBean}";
     
    private Logger logger = Logger.getLogger(ControlBean.class.getName());
    
    private ManagementService managementService;

    enum Action { STATE, START, STOP, SHUTDOWN, SHUTDOWNFORCE, SUSPEND, RESUME }

    
    /** Creates a new instance of ControlBean */
    public ControlBean() {
    }
    
    public TableDataProvider getComponents() {

        String state = invokeAction(Action.STATE);
                
        List<DisplayControl> list = new ArrayList<DisplayControl>();
        
        DisplayControl item = new DisplayControl(componentName,state,"");
        list.add(item);
        
        provider = new ObjectListDataProvider(list);
        
        return provider;
        
    }    
    
    
    public String start() {
        logger.info("start action");
        invokeAction(Action.START);
        return GenericConstants.SUCCESS;
    }

    public String stop() {
        logger.info("stop action");
        invokeAction(Action.STOP);
        return GenericConstants.SUCCESS;
    }
    
    public String shutdown() {
        logger.info("shutdown action");
        invokeAction(Action.SHUTDOWN);
        return GenericConstants.SUCCESS;
    }

    public String suspend() {
        logger.info("suspend action");
        invokeAction(Action.SUSPEND);
        return GenericConstants.SUCCESS;
    }
    
    public String resume() {
        logger.info("resume action");
        invokeAction(Action.RESUME);
        return GenericConstants.SUCCESS;
    }

    
    private String invokeAction(Action action) {
        
        String result = "";
        
        setup();
        if ( managementService==null ) {
            managementService = serviceManager.getManagementService(tName);
        }
        String name = componentName;
        String type = componentType;
        if ( GenericConstants.SU_TYPE.equals(type) ) {
            name = cName;
            type = cType;
        }
        
        switch(action) {
            case STATE:
                result = managementService.getState(name,type);
                break;
            case START:
                result = managementService.start(name,type);
                break;
            case STOP:
                result = managementService.stop(name,type);
                break;
            case SHUTDOWN:
                result = managementService.shutdown(name,type);
                break;
            case SHUTDOWNFORCE:
                result = managementService.shutdownForce(name,type);
                break;
            case SUSPEND:
                result = managementService.suspend(name,type);
                break;
            case RESUME:
                result = managementService.resume(name,type);
                break;
        }
        
        return result;
    }

        
    public String getTitle() {
        return getName()+" - " + Messages.getString("control_title");
    }
    
    public String getTableTitle() {
        return getTableTitle("control_bc_tabletitle","control_se_tabletitle","control_se_tabletitle");
    }    
}
