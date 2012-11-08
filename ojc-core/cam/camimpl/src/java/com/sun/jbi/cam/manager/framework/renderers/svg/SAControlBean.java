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
 * @(#)SAControlBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.renderers.svg;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.manager.framework.common.BaseBean;
import com.sun.jbi.cam.manager.framework.generic.ControlBean;
import com.sun.jbi.cam.manager.framework.generic.ServiceUnitsBean;
import com.sun.jbi.cam.model.management.JBIServiceUnitStatus;
import java.io.Serializable;
import java.util.List;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

/**
 *
 * @author rdamir
 */
public final class SAControlBean extends BaseBean implements Serializable {
    
    public static String BEAN_NAME = "#{SAControlBean}";

    private final static int START_BUTTON_STATUS_DONE = 1;
    private final static int STOP_BUTTON_STATUS_DONE = 2;
    private final static int SHUTDOWN_BUTTON_STATUS_DONE = 4;
    private final static int ALL_BUTTONS_STATUS_DONE = 7;
    private final static int NONE_BUTTONS_STATUS_DONE = 0;
    private final static String BUTTON_DISBALED = "disabled";
    private final static String BUTTON_ENBALED = "enabled";
    
    public static enum State { STARTED, STOPPED, SHUTDOWN, SUSPENDED, RUNNING }
    
    private int queryState = NONE_BUTTONS_STATUS_DONE;
    private boolean isLegendVisible = true;
    private String saState;
    private List<JBIServiceUnitStatus> suStatusList;
 
    private String startedState;
    private String stopedState;
    private String shutdownState;
    private String statusString;
    private int countStarted;
    private int countStopped;
    private int countShutdown;
    private boolean isTextView;
 
    
    /** Creates a new instance of SAControlBean */
    public SAControlBean() {
        setSessionAttributeVal();
        startedState = Messages.getString("state.Started");
        stopedState = Messages.getString("state.Stopped");
        shutdownState = Messages.getString("state.Shutdown");
        statusString = Messages.getString("sa_state");
        clearCounter();

    }
    
    public String getLegendControlString() {
        if(isLegendVisible) {
            return Messages.getString("control_hideLegendAction");
        }
        return Messages.getString("control_showLegendAction");
    }
    
    public void switchLegendVisibility() {
        isLegendVisible = !isLegendVisible;
        setSessionAttributeVal();

    }
    
    public boolean getLegendVisibility() {
        return isLegendVisible;
    }
    
    private void setSessionAttributeVal(){
        FacesContext context = FacesContext.getCurrentInstance();
        ExternalContext ex = context.getExternalContext();
        HttpServletRequest request = (HttpServletRequest)ex.getRequest();
        HttpSession session = request.getSession();  
        session.setAttribute("legendVisible",
                isLegendVisible==true ? "true" : "false");
       
    }
    
    
    
    public boolean isSAStartButtonDisabled() {
      performServiceUnitsStatusQuery();
      queryState |= START_BUTTON_STATUS_DONE;
      return figureOutButtonState(START_BUTTON_STATUS_DONE);
    }

    public boolean isSAStopButtonDisabled() {
       performServiceUnitsStatusQuery();
       queryState |= STOP_BUTTON_STATUS_DONE;
       return figureOutButtonState(STOP_BUTTON_STATUS_DONE);
        
    }

    public boolean isSAShutdownButtonDisabled() {
       performServiceUnitsStatusQuery();
       queryState |= SHUTDOWN_BUTTON_STATUS_DONE;
       return figureOutButtonState(SHUTDOWN_BUTTON_STATUS_DONE);
    }

    private void performServiceUnitsStatusQuery() {
        if(queryState == NONE_BUTTONS_STATUS_DONE) {
          clearCounter();
          this.getRequestParameters();
           ServiceUnitsBean susBean =  new ServiceUnitsBean();
            saState = susBean.getServiceAsseemblyState(componentName,tName);
            suStatusList = 
                    susBean.getSAServiceUnitStatusList(componentName,tName);
            for (JBIServiceUnitStatus sus : suStatusList) {
               String suStatus = sus.getStatus();
               if(suStatus.equals(stopedState)) {
                   countStopped++;
               }else if(suStatus.equals(startedState)) {
                   countStarted++;
               } else {
                   countShutdown++;
               }
                   
            }
        }
       
        
    }
    
    private boolean figureOutButtonState(int buttonType) {
        boolean state = false;
        switch (buttonType) {
            case START_BUTTON_STATUS_DONE:
                state = defineStartButtonDisableState();
                break;
            case STOP_BUTTON_STATUS_DONE:
                state = defineStopButtonDisableState();
                break;
            case SHUTDOWN_BUTTON_STATUS_DONE:
                state = defineShutdownButtonDisableState();
                break;
        }
        return state;
    }
    
    public boolean isSAQueryFlagCleared() {
        // allow new query to take place
        queryState = NONE_BUTTONS_STATUS_DONE;
        return true;
    }
    private boolean  defineStartButtonDisableState() {
        
        if(countStopped > 0 || countShutdown > 0) {
            return false;
        }
        // all su are started disable start button
       return  true;    
    }
    
     private boolean defineStopButtonDisableState() {
         if(countStarted > 0 ){
            return false;
        }
          // all su are stopped disable stop button
        return  true;    
    }
   

    private boolean defineShutdownButtonDisableState() {
         if(countStopped > 0 ){
            return false;
        }
          // all su are not it stopped state disable start button
        return  true;    
    }

    private void clearCounter() {
       countStarted = 0;
       countStopped = 0;
       countShutdown = 0;

    }
    
    public String getAssemblyStatus() {
        String state = null;
        if(suStatusList != null) {
            int suCount = suStatusList.size();
            
            if(suCount ==countStarted) {
               state = Messages.getString("sa_all_units_started");
            } else if(suCount == countStopped) {
               state = Messages.getString("sa_all_units_stopped");
            } else if(suCount == countShutdown) {
               state = Messages.getString("sa_all_units_shutdown");
            } else if(suCount == countStarted + countShutdown ) {
               state = Messages.getString("sa_unit_started_shutdown");
            } else if(suCount == countStarted + countStopped) {
               state = Messages.getString("sa_unit_started_stopped");
            } else if(suCount ==  countStopped + countShutdown ) {
               state = Messages.getString("sa_unit_stopped_shutdown");
            } else {
               state = Messages.getString("sa_unit_started_stopped_shutdown");
            } 
        } else {
            state = Messages.getString("sa_state_undefined");
        }
        String status= statusString + state;        
        return status;
    }
 
    public String switchView() {
        isTextView = !isTextView;
        return getNavigationValue();
   }

    
    
    public String getViewActionText() {
        if(isTextView) {
            return Messages.getString("switch_to_svg_view");
        }
        return Messages.getString("switch_to_text_view");
    }

    public boolean isTextView() {
        return isTextView;
    }
    
    public String getInlinehelp() {
        if(isTextView) {
            return Messages.getString("sa_jbixml_inlinehelp");
        }
        return Messages.getString("sa_unit_svg_inlinehelp");
        
    }
    
    public String getTargetView() {
        if(isTextView) {
           return GenericConstants.SERVICE_ASSEMBLY_TEXTVIEWER_URL;
        }
        return GenericConstants.SERVICE_ASSEMBLY_SPLITVIEWER_URL;
        
    }
    
    public String start() {
        javax.faces.context.FacesContext context = 
        javax.faces.context.FacesContext.getCurrentInstance();

        javax.faces.el.ValueBinding controlBinding = 
        context.getApplication().createValueBinding(ControlBean.BEAN_NAME);
        
        ControlBean controlBean = (ControlBean)controlBinding.getValue(context);
        controlBean.start();
        return getNavigationValue();

    }
    public String stop() {
        javax.faces.context.FacesContext context = 
        javax.faces.context.FacesContext.getCurrentInstance();

        javax.faces.el.ValueBinding controlBinding = 
        context.getApplication().createValueBinding(ControlBean.BEAN_NAME);

        ControlBean controlBean = (ControlBean)controlBinding.getValue(context);
        controlBean.stop();
        return getNavigationValue();
        
    }

    public String shutdown() {
         javax.faces.context.FacesContext context = 
        javax.faces.context.FacesContext.getCurrentInstance();

        javax.faces.el.ValueBinding controlBinding = 
        context.getApplication().createValueBinding(ControlBean.BEAN_NAME);

        ControlBean controlBean = (ControlBean)controlBinding.getValue(context);
        controlBean.shutdown();
        return getNavigationValue();
       
    }


    
    private String getNavigationValue(){
        if(isTextView) {
            return "toTextView";
        }
        return "toSVGView";

    }
}
