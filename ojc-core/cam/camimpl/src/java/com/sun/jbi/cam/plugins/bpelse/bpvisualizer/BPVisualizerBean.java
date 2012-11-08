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
 * @(#)BPVisualizerBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.bpvisualizer;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.manager.framework.generic.DisplayServiceUnits;
import com.sun.jbi.cam.manager.framework.generic.ServiceUnitsBean;
import com.sun.webui.jsf.component.Checkbox;
import com.sun.webui.jsf.component.DropDown;
import com.sun.webui.jsf.model.Option;
import java.util.ArrayList;
import java.util.List;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import javax.faces.event.ValueChangeEvent;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import org.jfree.data.general.Dataset;

/**
 *
 * @author rdamir
 */
public class BPVisualizerBean extends ChartingConstants {
    
    private static String SELECT_SERVICE_UNIT_STRING;
    private static String SE_TITLE_STRING;
    private static String SUS_TITLE_STRING;
    private static String SU_OPTION_DROP_DOWN_LABEL;
    private static String CHART_OPTION_DROP_DOWN_LABEL;

    static {
        SELECT_SERVICE_UNIT_STRING  = 
        Messages.getString("bpvisualizer_Bpel_select_su");
        SE_TITLE_STRING  = 
        Messages.getString("bpvisualizer_Bpel_se_title");
        SUS_TITLE_STRING  = 
        Messages.getString("bpvisualizer_Bpel_sus_title");
        SU_OPTION_DROP_DOWN_LABEL = 
            Messages.getString("bpvisualizer_suinstances_label");
        CHART_OPTION_DROP_DOWN_LABEL = 
            Messages.getString("bpvisualizer_chartoptions_label");
    }

    public enum BPViews { NOTSET,SEVIEW ,SUVIEW};
    public static String BEAN_NAME = "#{BPVisualizer}";

    private Option[] ChartSelectionMenuOptions;
    private Option[] serviceUnitInstanceSelectionOptions = null;
    private String serviceUnitInstanceSelected;
    
    private boolean aggregatedView = true;
    private BPViews lastView = BPViews.NOTSET;
    private String ChartSelectionMenuSelectedOption;
    private Option[] suChartSelectionMenuOptions;
    private Option[] seChartSelectionMenuOptions;
  
    ChartSupport currentChart;
    
    private List<String> suNameList;
    /** Creates a new instance of BPVisualizerBean */
    public BPVisualizerBean() {
         suNameList = new ArrayList<String>();
         seChartSelectionMenuOptions = new Option[2]; 
         seChartSelectionMenuOptions[0] = new Option(SELECT_CHART_STRING);
         seChartSelectionMenuOptions[1] = new Option(BPS_AGGREGATED_STATUS, 
                   this.BPS_AGGREGATED_STATUS_DISPLAY);
         suChartSelectionMenuOptions = new Option[3]; 
         suChartSelectionMenuOptions[0] = new Option(SELECT_CHART_STRING);
         suChartSelectionMenuOptions[1] = new Option(BP_SERVICE_UNIT_STATUS, 
                   this.BP_SERVICE_UNIT_STATUS_DISPLAY);
         suChartSelectionMenuOptions[2] = new Option(BP_SERVICE_UNIT_CUSTOM, 
                  this.BP_SERVICE_UNIT_CUSTOM_DISPLAY);
          setDefaultChartSelectionMenuItem();
       
    }
    
    
    
    public String getServiceUnitInstanceSelected() {
        return serviceUnitInstanceSelected;
    }
    
    public void setserviceUnitInstanceSelected(String selected) {
        this.serviceUnitInstanceSelected = selected;
    }
   
    public Option[] getServiceUnitInstanceSelectionOptions() {
        serviceUnitInstanceSelectionOptions = getSUInstancesList();
        return serviceUnitInstanceSelectionOptions;
    }
 
    public void setBpelInstanceOptions(Option[] bpelSUInstanceOptions) {
        this.serviceUnitInstanceSelectionOptions = bpelSUInstanceOptions;
    }

    
    
  
    private Option[] getSUInstancesList() {
        suNameList.clear();
        ServiceUnitsBean srvUnitsBean =  new ServiceUnitsBean();
        List<DisplayServiceUnits> list = srvUnitsBean.getServiceUnitList(null);
        Option[]  options = new Option[list.size()+1];
        options[0] = new Option(SELECT_SERVICE_UNIT_STRING);
        for (int index = 1; index <= list.size(); index++) {
            DisplayServiceUnits su = list.get(index-1);
            String suName = su.getName();
            options[index] = new Option(suName,suName); 
            suNameList.add(suName);
        }
        return options;
    }
    
    public void switchView() {
        aggregatedView = !aggregatedView;
        if(!aggregatedView) {
            this.setserviceUnitInstanceSelected(SELECT_SERVICE_UNIT_STRING); 
        }
        setDefaultChartSelectionMenuItem();
       
    }
    
    public String getViewActionText() {
        if(aggregatedView) {
            return Messages.getString("bpvisualizer_instanceview_label");
        }
        return Messages.getString("bpvisualizer_aggregatedview_label");
   }

   public String getInLineHelpText() {
        if(isAggregatedView()) {
            return Messages.getString("bpvisualizer_Bpel_select_se_label");
        }
        return Messages.getString("bpvisualizer_Bpel_select_su_label");
   }

    
    public boolean isAggregatedView() {
        BPViews currentView = getCurrentView();
        if(currentView != lastView) {
          lastView = currentView;
          if(currentView == BPViews.SUVIEW) {
              // user can reach this point in the following cases
              // 1. first time the bpvisualer is selected from the su tree node
              // 2. the user toggle to it from the bpelse se tree node
                // update service unit name
                updateServiceUnitName();
                //set chart option for su
                ChartSelectionMenuOptions = suChartSelectionMenuOptions;
                aggregatedView = false;
          } else if(currentView == BPViews.SEVIEW) {
              // user can reach this point in the following cases
              // 1. first time the bpvisualer is selected from the se tree node
              // 2. the user toggle to it from the bpelse se tree node
              ChartSelectionMenuOptions = seChartSelectionMenuOptions;
               aggregatedView =  true;
          }
        } else {
           // user can reach this point in the following cases
           // 1. the user move from one su node to another
           if(currentView == BPViews.SUVIEW) {
                // update service unit name
                updateServiceUnitName();
                //set chart option for su
                ChartSelectionMenuOptions = suChartSelectionMenuOptions;
                aggregatedView = false;
            }
        }
        return aggregatedView;
    }
    
    private void updateServiceUnitName() {
        // update service unit name
        FacesContext context = FacesContext.getCurrentInstance();
        ExternalContext ex = context.getExternalContext();
        HttpServletRequest request = (HttpServletRequest)ex.getRequest();
        HttpSession session = request.getSession();  
        String name = 
          (String)session.getAttribute(GenericConstants.COMPONENT_NAME);
        String parentName = 
          (String)session.getAttribute(GenericConstants.COMPONENT_PNAME);
        serviceUnitInstanceSelected = parentName + "-" + name;
      
    }
    
    public Dataset getChartData() {
        // get the latest set of su
        getSUInstancesList();
        currentChart.setServiceUnitsList(suNameList);
        currentChart.setAggregatedView(aggregatedView);  
        currentChart.setServiceUnitSelected(serviceUnitInstanceSelected); 
        return currentChart.getChartData();
    }
    
    public BPViews getCurrentView( ) {
        FacesContext context = FacesContext.getCurrentInstance();
        ExternalContext ex = context.getExternalContext();
        HttpServletRequest request = (HttpServletRequest)ex.getRequest();
        HttpSession session = request.getSession();  
        String viewType = 
                (String)session.getAttribute(GenericConstants.COMPONENT_TYPE);
        if(viewType.equals(GenericConstants.SU_TYPE)) {
            return BPViews.SUVIEW;
        }
        return  BPViews.SEVIEW;
    }

    
    public String getChartTitle() {
         String title = "";
          if(aggregatedView == false) {
             if(serviceUnitInstanceSelected != null && 
                     serviceUnitInstanceSelected.length() > 0) {
                 if(ChartSelectionMenuSelectedOption.equals(BP_SERVICE_UNIT_STATUS)) {
                     title = serviceUnitInstanceSelected + 
                             Messages.getString("bpvisualizer_Bpel_chart_title_status_su");
                 } else  if(ChartSelectionMenuSelectedOption.equals(BP_SERVICE_UNIT_CUSTOM)) {
                      title = serviceUnitInstanceSelected + 
                             Messages.getString("bpvisualizer_Bpel_chart_title_custom_su");
                 }
             }
          } else {
             if(ChartSelectionMenuSelectedOption.equals(BPS_AGGREGATED_STATUS)) {
                 title = Messages.getString("bpvisualizer_Bpel_chart_title_status_all");
             }

          }
          return title;
    }

    
    public boolean getIsThreeDimensionalEffect() {
        return currentChart.getIsThreeDimensionalEffect();
    }    
    
    public void setIsThreeDimensionalEffect(boolean isThreeDimensionalEffect) {
       currentChart.setIsThreeDimensionalEffect(isThreeDimensionalEffect);
   }

    public String getChartMenuType() {
        return currentChart.getChartMenuType();
    }
    
    public void setChartMenuType(String chartMenuType) {
       currentChart.setChartMenuType(chartMenuType);
    }

    public String getGraphOrientation() {
        return currentChart.getGraphOrientation();
    }
    
    public void setGraphOrientation(String orientationOfGraph) {
       currentChart.setGraphOrientation(orientationOfGraph);
    }

    
    public void setChartTypeMenuSelectedOption(String selected) {
        currentChart.setChartTypeMenuSelectedOption(selected);
    }
    
    public String getChartTypeMenuSelectedOption() {
         return currentChart.getChartTypeMenuSelectedOption();
    }
    
    
    public Option[] getChartMenuTypeOptions() {
        if(currentChart == null) {
            Option[] defaultOp = new Option[1];
            defaultOp[0] = new Option(SELECT_CHART_STRING);
            return defaultOp;
        }
        return currentChart.getChartMenuTypeOptions();
    }
    
    public Option[] getChartSelectionMenuOptions() {
        
        if(aggregatedView) {
           ChartSelectionMenuOptions = seChartSelectionMenuOptions; 
        } else {
           ChartSelectionMenuOptions =suChartSelectionMenuOptions; 
        }
        return ChartSelectionMenuOptions;
    }

    public void setChartSelectionMenuSelectedOption(String selected) {
        ChartSelectionMenuSelectedOption = selected;
        if(ChartSelectionMenuSelectedOption.equals(BPS_AGGREGATED_STATUS) ||
           ChartSelectionMenuSelectedOption.equals(BP_SERVICE_UNIT_STATUS)){
            if(currentChart instanceof BPStatusCharts == false) {
                 currentChart = new BPStatusCharts();
            }
            if(ChartSelectionMenuSelectedOption.equals(BPS_AGGREGATED_STATUS)) {
                ((BPStatusCharts)currentChart).setChartTypeOptions(true);
            } else {
                ((BPStatusCharts)currentChart).setChartTypeOptions(false);
            }
        } else if(ChartSelectionMenuSelectedOption.equals(BP_SERVICE_UNIT_CUSTOM)) {
            if(currentChart instanceof BPCustomCharts == false) {
                 currentChart = new BPCustomCharts();
            }
        }
   }
    public String getChartSelectionMenuSelectedOption() {
         return ChartSelectionMenuSelectedOption;
    }
    
    public void processChartSelectionMenuSelection(ActionEvent actionEvent) {
        // Since the action is immediate, the components won't go through the
        // Update Model phase.  So, we need to explicitly set the values and
        // update the model object for the given action selected.
        FacesContext context = FacesContext.getCurrentInstance();
        DropDown dropDown = (DropDown) actionEvent.getComponent();
        String selected = (String) dropDown.getSelected();
        this.setChartSelectionMenuSelectedOption(selected);
        if(selected.equals(BPS_AGGREGATED_STATUS) || 
                selected.equals(BP_SERVICE_UNIT_STATUS)) {
        }
    }  
    public void processChartTypeMenuSelection(ActionEvent actionEvent) {
        currentChart.processChartTypeMenuSelection(actionEvent);
    }  
   
    
    
     public String getIsThreeDimensional() {
         return currentChart.getIsThreeDimensional();
   }
    
    public void setIsThreeDimensional(String isThreeDimensional) {
        currentChart.setIsThreeDimensional(isThreeDimensional);
    }
    
    public boolean isThreeDimensionalEffect() {
         return currentChart.isThreeDimensionalEffect();
   }
    
    public boolean isHorizontalOrientation() {
         return currentChart.isHorizontalOrientation();
    }
    
    public boolean getIsHorizontalOrientation() {
         return currentChart.isHorizontalOrientation();
    }
    
    public void setIsHorizontalOrientation(boolean isHorizontalOrientation) {
        currentChart.setIsHorizontalOrientation(isHorizontalOrientation);
    }
    
    public String getThreeDimensional() {
        return currentChart.getThreeDimensional();
    }
    
   public String getTwoDimensional() {
         return currentChart.getTwoDimensional();
   }
    
    public String getOrientationVertical() {
         return currentChart.getOrientationVertical();
    }
    
    public String getOrientationHorizontal() {
        return currentChart.getOrientationHorizontal();
    }

    
    public void effectChanged(ValueChangeEvent valueChangeEvent) {
        FacesContext context = FacesContext.getCurrentInstance();
        Checkbox checkbox = (Checkbox) valueChangeEvent.getComponent();
        Boolean selected = (Boolean) checkbox.getSelected();
        if(selected.booleanValue() == true) {
            setIsThreeDimensionalEffect(true);
            setIsThreeDimensional(Boolean.TRUE.toString());
        } else {
            setIsThreeDimensionalEffect(false);
            setIsThreeDimensional(Boolean.FALSE.toString());
        }
    }
    
    public void orientationChanged(ValueChangeEvent valueChangeEvent) {
        FacesContext context = FacesContext.getCurrentInstance();
        Checkbox checkbox = (Checkbox) valueChangeEvent.getComponent();
        Boolean selected = (Boolean) checkbox.getSelected();
        if(selected.booleanValue() == true) {
           setIsHorizontalOrientation(true);
           setGraphOrientation("horizontal");
        } else {
            setIsHorizontalOrientation(false);
            setGraphOrientation("vertical");
        }
    }
    
    public void checkBoxChanged(ValueChangeEvent valueChangeEvent) {
        FacesContext context = FacesContext.getCurrentInstance();
        Checkbox checkbox = (Checkbox) valueChangeEvent.getComponent();
        if(checkbox.getLabel().equals(getThreeDimensional()) == true) {
            this.effectChanged(valueChangeEvent);
        }
        if(checkbox.getLabel().equals(getOrientationHorizontal()) == true) {
            this.orientationChanged(valueChangeEvent);
        }
    }
    
    public void processServiceUnitInstanceSelection(ActionEvent actionEvent) {
        // Since the action is immediate, the components won't go through the
        // Update Model phase.  So, we need to explicitly set the values and
        // update the model object for the given action selected.
        FacesContext context = FacesContext.getCurrentInstance();
        DropDown dropDown = (DropDown) actionEvent.getComponent();
        String selected = (String) dropDown.getSelected();
        serviceUnitInstanceSelected = selected;
    }  
    
    private void setDefaultChartSelectionMenuItem() {
       setChartSelectionMenuSelectedOption(this.SELECT_CHART_STRING);

    }
    
    public String getPageTitle() {
        if(aggregatedView) {
            return this.SE_TITLE_STRING;
        }
        if(SELECT_SERVICE_UNIT_STRING.equals(serviceUnitInstanceSelected)) {
            return SUS_TITLE_STRING;
        }
        
        return this.SUS_TITLE_STRING + " (" + serviceUnitInstanceSelected + 
                ")";
    }
    
    public String getInstanceSelectorLabel() {
        return SU_OPTION_DROP_DOWN_LABEL;
    }
    
    public String getChartSelectorLabel() {
        return CHART_OPTION_DROP_DOWN_LABEL;
    }
    
}
