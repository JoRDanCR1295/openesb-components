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
 * @(#)BPCustomChartBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.bpvisualizer;

import com.sun.jbi.cam.plugins.bpelse.BPELModelProcessor;
import com.sun.jbi.cam.plugins.bpelse.common.BpelsePluginMessages; 
import com.sun.webui.jsf.component.DropDown;
import com.sun.webui.jsf.model.Option;
import java.util.Date;
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Logger;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import org.apache.commons.validator.routines.DoubleValidator;
import org.apache.commons.validator.routines.IntegerValidator;
import org.apache.commons.validator.routines.LongValidator;
import org.apache.commons.validator.routines.FloatValidator;
import org.apache.commons.validator.routines.DateValidator;


/**
 *
 * @author rdamir
 */
public class BPCustomChartBean extends ChartingConstants {
    
    public static int BUFFER_SIZE = 4096;
    private Logger logger = Logger.getLogger(BPCustomChartBean.class.getName());
    public static String BEAN_NAME = "#{CustomChartBean}";
    
    private String selectedBpelProcess;
    private String selectedXVariable;
    private String selectedYVariable;
    private BPVisualizerBean visualizerBean;
    private String currentServiceUnit= null;
    // map to hold bpel name to list of 2 map entries 
    // (var2idmap & id2varmap) mapping
    private Map<String,List<Map<String,String>>> bpelProcessesMap;
    private Map<String,Map<String,VAR_TYPE>> bpelName2id2TypeMap;
    
    public enum VAR_TYPE { BOOLEAN, NUMERIC ,STRING,DATETIME,NA};
    enum VAR_COMBINATION { NUMERIC_NUMERIC,NUMERIC_STRING,
        NUMERIC_DATETIME,NUMERIC_BOOLEAN,NA};

    private VAR_TYPE selectedXVariableType;
    private VAR_TYPE selectedYVariableType;
    
    private String lowerLimit_x_axis;
    private String upperLimit_x_axis;
    private String lowerLimit_y_axis;
    private String upperLimit_y_axis;
    
    private Date dateLowerLimit_x_axis;
    private Date dateUpperLimit_x_axis;
    private Date dateLowerLimit_y_axis;
    private Date dateUpperLimit_y_axis;
    
    private boolean xDateRendered;
    private boolean yDateRendered;
    private boolean xNumericRendered;
    private boolean yNumericRendered;
    private boolean xVariableLimitsDisabled;
    private boolean yVariableLimitsDisabled;
    private boolean customPlotRequested;
    private boolean hasValidEntries;
    private boolean clearSelectionRequested;
    
    private StringBuffer validationErrorBuffer;
    private boolean isChartSelected;

    
    /** Creates a new instance of BPCustomChartBean */
    public BPCustomChartBean() {
        clearCustomSelection();
        javax.faces.context.FacesContext context =
                javax.faces.context.FacesContext.getCurrentInstance();
        
        javax.faces.el.ValueBinding valueBinding =
                context.getApplication().createValueBinding(BPVisualizerBean.BEAN_NAME);
        
        visualizerBean =
                (BPVisualizerBean) valueBinding.getValue(context);
    }
    
    public boolean isChartSelected() {
        isChartSelected = !visualizerBean.getChartTypeMenuSelectedOption().equals(SELECT_CHART_STRING)
            && !clearSelectionRequested;
        return  isChartSelected;
    }
    
    public boolean getXVariableLimitsDisabled() {
        xVariableLimitsDisabled = isXVariableHasLimit()==true?false:true;
        return xVariableLimitsDisabled;
    }
    
    public boolean getYVariableLimitsDisabled() {
        yVariableLimitsDisabled = isYVariableHasLimit()==true?false:true;
        return yVariableLimitsDisabled;
    }
    
    public VAR_TYPE getCurrentXVaribleType() {
        return selectedXVariableType;
    }
    
    public VAR_TYPE getCurrentYVaribleType() {
        return selectedYVariableType;
    }
    
    public String getLowerLimit_X_Axis() {
        return lowerLimit_x_axis == null ? "" : lowerLimit_x_axis;
    }
    
    public void setLowerLimit_X_Axis(String limit) {
        lowerLimit_x_axis = limit;
    }
    
    
    public String getUpperLimit_X_Axis() {
        return upperLimit_x_axis == null ? "" : upperLimit_x_axis;
    }
    
    public void setUpperLimit_X_Axis(String limit) {
        upperLimit_x_axis = limit;
        
    }
    
    public String getLowerLimit_Y_Axis() {
        return lowerLimit_y_axis == null ? "" : lowerLimit_y_axis;
    }
    
    public void setLowerLimit_Y_Axis(String limit) {
        lowerLimit_y_axis = limit;
        
    }
    
    
    public String getUpperLimit_Y_Axis() {
        return upperLimit_y_axis == null ? "" : upperLimit_y_axis;
    }
    
    public void setUpperLimit_Y_Axis(String limit) {
        upperLimit_y_axis = limit;
        
    }
    
    public Date getDateLowerLimit_X_Axis() {
        return dateLowerLimit_x_axis;
    }
    
    public void setDateLowerLimit_X_Axis(Date limit) {
        dateLowerLimit_x_axis = limit;
    }
    
    
    public Date getDateUpperLimit_X_Axis() {
        return dateUpperLimit_x_axis;
    }
    
    public void setDateUpperLimit_X_Axis(Date limit) {
        dateUpperLimit_x_axis = limit;
        
    }
    
    public Date getDateLowerLimit_Y_Axis() {
        return dateLowerLimit_y_axis;
    }
    
    public void setDateLowerLimit_Y_Axis(Date limit) {
        dateLowerLimit_y_axis = limit;
        
    }
    
    
    public Date getDateUpperLimit_Y_Axis() {
        return dateUpperLimit_y_axis;
    }
    
    public void setDateUpperLimit_Y_Axis(Date limit) {
        dateUpperLimit_y_axis = limit;
    }
       
    
    public boolean hasSimpleVars() {
        if(bpelProcessesMap == null || bpelProcessesMap.size() == 0) {
            return false;
        }
        return true;
    }
    
    public String getAxis_X_SelectedOption() {
        return selectedXVariable;
    }
    
    public String getBpelProcessSelectedOption() {
        return selectedBpelProcess;
    }
    
    public void setBpelProcessSelectedOption(String selected) {
        selectedBpelProcess = selected;
    }

    
    public Option[] getBpelProcessSelectionMenuOptions() throws Exception{
        checkServiceUnitChanged();
        Option[]  bpelProcessSelection = null;
        if(bpelProcessesMap != null) {
            int bpelProcessesCount = bpelProcessesMap.size();
            switch (bpelProcessesCount) {
            case 0:
                throw new Exception(BpelsePluginMessages.getString("bpvisualizer_no_bpelProcess_found"));
            case 1:
                bpelProcessSelection = new Option[1];
                String bpelProcessId =  (String)bpelProcessesMap.keySet().toArray()[0];
                bpelProcessSelection[0] = new Option(bpelProcessId,bpelProcessId);
                selectedBpelProcess = bpelProcessId;
                break;
            default:
                bpelProcessSelection = new Option[bpelProcessesCount+1];
                int optionIndex =0;
                bpelProcessSelection[optionIndex++] =  new Option(SELECT_BPEL_PROCESS);
                for (String bpelName : bpelProcessesMap.keySet()) {
                    bpelProcessSelection[optionIndex++] =  new Option(bpelName,bpelName);
                }
                break;
            }
        } else {
            throw new Exception(BpelsePluginMessages.getString("bpvisualizer_no_bpelProcess_found"));
        }
        return bpelProcessSelection;
    }
    
    public String getAxis_Y_SelectedOption() {
        return selectedYVariable;
    }
    
    public void setAxis_X_SelectedOption(String selected) {
        selectedXVariable = selected;
        if(bpelName2id2TypeMap.size() !=0) {
            Map<String,VAR_TYPE> id2VariableTypeMap = bpelName2id2TypeMap.get(this.selectedBpelProcess);
            selectedXVariableType = id2VariableTypeMap.get(selected) != null ?
                id2VariableTypeMap.get(selected) : VAR_TYPE.NA;
        }
    }
    
    public void setAxis_Y_SelectedOption(String selected) {
        selectedYVariable= selected;
        if(bpelName2id2TypeMap.size() != 0) {
            Map<String,VAR_TYPE> id2VariableTypeMap = bpelName2id2TypeMap.get(this.selectedBpelProcess);
            selectedYVariableType = id2VariableTypeMap.get(selected) != null ?
                id2VariableTypeMap.get(selected) : VAR_TYPE.NA;
        }
    }
    
    
    public Option[] getAxis_X_SelectionMenuOptions() throws Exception {
        checkServiceUnitChanged();
        Map<String,String> tempVariable2IdMap= new HashMap<String,String>();
        VAR_TYPE yType = getCurrentYVaribleType();
        if(bpelProcessesMap == null || selectedBpelProcess == null || 
                selectedBpelProcess.equals(SELECT_BPEL_PROCESS)) {
            return buildOptionList(tempVariable2IdMap);
        }
        List<Map<String,String>> mapsList = bpelProcessesMap.get(this.selectedBpelProcess);
        Map<String,String> variable2IdMap = mapsList.get(0);
        Map<String,String> id2VariableMap = mapsList.get(1);
        Map<String,VAR_TYPE> id2VariableTypeMap = bpelName2id2TypeMap.get(this.selectedBpelProcess);
        if(yType == VAR_TYPE.NA ||  yType == VAR_TYPE.NUMERIC) {
            // no y selection or its numeric x can be any valid var.
            tempVariable2IdMap = cloneMap(variable2IdMap);
            if(yType == VAR_TYPE.NUMERIC) {
                // remove the selected y variable from x variables list
                tempVariable2IdMap.remove(id2VariableMap.get(selectedYVariable));
            }
        } else {
            // filter out selection if any
            for (String varName : variable2IdMap.keySet()) {
                String id = variable2IdMap.get(varName);
                // remove the selected y variable from x variables list
                if(id.equals(selectedYVariable)) {
                    continue;
                }
                // only numric var are allowed
                VAR_TYPE xType = id2VariableTypeMap.get(id);
                if(xType != VAR_TYPE.NUMERIC) {
                    continue;
                }
                tempVariable2IdMap.put(varName,id);
            }
        }
        Option[] varsOptions = buildOptionList(tempVariable2IdMap);
        return varsOptions;
        
    }
    
    public Option[] getAxis_Y_SelectionMenuOptions() throws Exception {
        checkServiceUnitChanged();
        Map<String,String> tempVariable2IdMap= new HashMap<String,String>();
        VAR_TYPE xType = getCurrentXVaribleType();
        if(bpelProcessesMap == null || selectedBpelProcess == null || 
                selectedBpelProcess.equals(SELECT_BPEL_PROCESS )) {
            return buildOptionList(tempVariable2IdMap);
        }
        List<Map<String,String>> mapsList = bpelProcessesMap.get(this.selectedBpelProcess);
        Map<String,String> variable2IdMap = mapsList.get(0);
        Map<String,String> id2VariableMap = mapsList.get(1);
        Map<String,VAR_TYPE> id2VariableTypeMap = bpelName2id2TypeMap.get(this.selectedBpelProcess);
        if(xType == VAR_TYPE.NA ||  xType == VAR_TYPE.NUMERIC) {
            // no x selection or its numeric y can be any valid var.
            tempVariable2IdMap = cloneMap(variable2IdMap);
            if(xType == VAR_TYPE.NUMERIC) {
                // remove the selected x variable from y variables list
                tempVariable2IdMap.remove(id2VariableMap.get(selectedXVariable));
            }
        } else {
            // filter out selection if any
            for (String varName : variable2IdMap.keySet()) {
                String id = variable2IdMap.get(varName);
                // remove the selected x variable from y variables list
                if(id.equals(selectedXVariable)) {
                    continue;
                }
                VAR_TYPE yType = id2VariableTypeMap.get(id);
                if(yType != VAR_TYPE.NUMERIC ) {
                    continue;
                }
                tempVariable2IdMap.put(varName,id);
            }
        }
        
        Option[] varsOptions = buildOptionList(tempVariable2IdMap);
        return varsOptions;
        
    }
    
    
    public void process_X_AxisSelectionMenuSelection(ActionEvent actionEvent) {
        // Since the action is immediate, the components won't go through the
        // Update Model phase.  So, we need to explicitly set the values and
        // update the model object for the given action selected.
        FacesContext context = FacesContext.getCurrentInstance();
        DropDown dropDown = (DropDown) actionEvent.getComponent();
        String selected = (String) dropDown.getSelected();
        this.setAxis_X_SelectedOption(selected);
        // clear the selected chart type
        this.visualizerBean.setChartTypeMenuSelectedOption(SELECT_CHART_STRING);
        // change in selection invalidate plot request
        customPlotRequested = false;
    }
    
    public void process_Y_AxisSelectionMenuSelection(ActionEvent actionEvent) {
        // Since the action is immediate, the components won't go through the
        // Update Model phase.  So, we need to explicitly set the values and
        // update the model object for the given action selected.
        FacesContext context = FacesContext.getCurrentInstance();
        DropDown dropDown = (DropDown) actionEvent.getComponent();
        String selected = (String) dropDown.getSelected();
        this.setAxis_Y_SelectedOption(selected);
        // clear the selected chart type
        this.visualizerBean.setChartTypeMenuSelectedOption(SELECT_CHART_STRING);
        // change in selection invalidate plot request
        customPlotRequested = false;
    }
    
    public void processBpelProcessSelectionMenuSelection(ActionEvent actionEvent) {
        // Since the action is immediate, the components won't go through the
        // Update Model phase.  So, we need to explicitly set the values and
        // update the model object for the given action selected.
        FacesContext context = FacesContext.getCurrentInstance();
        DropDown dropDown = (DropDown) actionEvent.getComponent();
        String selected = (String) dropDown.getSelected();
        this.setBpelProcessSelectedOption(selected);
    }
    
    public void clearCustomSelection() {
        selectedBpelProcess = null;
        selectedXVariable = null;
        selectedYVariable = null;
        selectedXVariableType = VAR_TYPE.NA;
        selectedYVariableType = VAR_TYPE.NA;
        hasValidEntries = true;
        validationErrorBuffer = null;
        clearSelectionRequested = true;
        customPlotRequested = false;
        
    }
    
    public boolean hasValidData(){
        return hasValidEntries;
    }
    
    
    public boolean wasCutomPlotRequested() {
        return customPlotRequested && !visualizerBean.isAggregatedView();
    }
    
    public void plotCustomChart() {
        clearSelectionRequested = false;
        validationErrorBuffer = new StringBuffer();
        // Validate selection prior to setting the
        // customPlotRequested to true.
        ValidateEntries();
        if(hasValidEntries == true) {
            
            customPlotRequested = true;
        } else {
            customPlotRequested = false;
        }
    }
    
   
    
    public void checkServiceUnitChanged() throws Exception {
        boolean sameUnit = true;
        if(currentServiceUnit == null)  {
            sameUnit = false;
        } else {
            sameUnit = currentServiceUnit.equals(visualizerBean.getServiceUnitInstanceSelected());
        }

        if(sameUnit == false) {
            currentServiceUnit = visualizerBean.getServiceUnitInstanceSelected();
            clearCustomSelection();
            // refresh variable map
            createVariablesMaps(currentServiceUnit);
        }
        
    }
   
    public String getValidationErrorMessage() {
        return validationErrorBuffer.toString();
    }
    
 
    
   public  String getVarName(String compositeVarName) {
       if(compositeVarName.indexOf(selectedBpelProcess)!= -1) {
         return compositeVarName.substring(selectedBpelProcess.length()+1);
       }
       // the name is not in the format of bpelname.varName
       return compositeVarName;
   }
    
   public String getChartXLabel() {
       List<Map<String,String>> mapsList = bpelProcessesMap.get(this.selectedBpelProcess);
       Map<String,String> id2VariableMap = mapsList.get(1);
       String xVar = id2VariableMap.get(selectedXVariable);
       return  getVarName(xVar);
   }
   
   public String getChartYLabel() {
       List<Map<String,String>> mapsList = bpelProcessesMap.get(this.selectedBpelProcess);
       Map<String,String> id2VariableMap = mapsList.get(1);
       String yVar = id2VariableMap.get(selectedYVariable);
       return  getVarName(yVar);
   }
    
    private Option[] buildOptionList(Map<String,String> map) {
        int index = 0;
        Option[] varsOptions = new Option[1+map.size()];
        varsOptions[index++] = new Option(SELECT_VAR);
        if(bpelProcessesMap != null && bpelProcessesMap.size() > 0 &&
            selectedBpelProcess != null && !selectedBpelProcess.equals(SELECT_BPEL_PROCESS)) {
            List<Map<String,String>> mapsList = bpelProcessesMap.get(this.selectedBpelProcess);
            Map<String,String> variable2IdMap = mapsList.get(0);
            for (String varName : map.keySet()) {
                String id = variable2IdMap.get(varName);
                varsOptions[index++] = new Option(id,varName);
            }
        }
        return varsOptions;
    }
    
    public boolean isXVariableHasLimit() {
        if((selectedXVariableType == VAR_TYPE.NUMERIC) ||
                (selectedXVariableType == VAR_TYPE.DATETIME)) {
            return true;
        }
        return false;
    }
    
    public boolean isYVariableHasLimit() {
        if((selectedYVariableType == VAR_TYPE.NUMERIC) ||
                (selectedYVariableType == VAR_TYPE.DATETIME)) {
            return true;
        }
        return false;
    }
    
    
    
    public boolean getXDateRendered() {
        xDateRendered =  isXVariableHasLimit()  &&
                (getCurrentXVaribleType() == VAR_TYPE.DATETIME);
        return xDateRendered;
    }
    
    public boolean getYDateRendered() {
        yDateRendered =  isYVariableHasLimit()  &&
                (getCurrentYVaribleType() == VAR_TYPE.DATETIME);
        return yDateRendered;
       
    }
            
     public boolean getXNumericRendered() {
         xNumericRendered =  isXVariableHasLimit()  &&
                (getCurrentXVaribleType() == VAR_TYPE.NUMERIC);
         return xNumericRendered;
      
    }
     
    public boolean getYNumericRendered() {
        yNumericRendered = isYVariableHasLimit()  &&
                (getCurrentYVaribleType() == VAR_TYPE.NUMERIC);
          return yNumericRendered;
       
    }
   
    
    private  void createVariablesMaps(String suName) throws Exception {

        BPELModelProcessor modelProcessor = BPELModelProcessor.getInstance();
        BPELProcessMaps  bpMaps = modelProcessor.createVariablesMaps(suName);
        bpelProcessesMap = bpMaps.getBpelProcessesMap();
        bpelName2id2TypeMap = bpMaps.getBpelName2id2TypeMap();
    }    
    
     
    private void ValidateEntries() {
        
        
        hasValidEntries = isVariablesSelected();
        if(!hasValidEntries) {
            return;
        }
        if(selectedXVariableType == VAR_TYPE.NUMERIC) {
            validateNumericEntry(true);
        } else  if(selectedXVariableType == VAR_TYPE.DATETIME) {
            validateDateEntry(true);
        }
        
        if(selectedYVariableType == VAR_TYPE.NUMERIC) {
            validateNumericEntry(false);
        } else  if(selectedYVariableType == VAR_TYPE.DATETIME) {
            validateDateEntry(false);
        }
        
    }
    
    private boolean isVariablesSelected() {
        boolean xValid = true;
        boolean yValid = true;
        String errorString =
                BpelsePluginMessages.getString("bpvisualizer_custom_define_var");
        if(selectedXVariable == null ||
                selectedXVariable.equals(SELECT_VAR)) {
            xValid = false;
            validationErrorBuffer.append(errorString);
            validationErrorBuffer.append(BpelsePluginMessages.getString("bpvisualizer_custom_x_axis"));
            validationErrorBuffer.append("<br/>");
        }
        if(selectedYVariable == null ||
                selectedYVariable.equals(SELECT_VAR)) {
            yValid = false;
            validationErrorBuffer.append(errorString);
            validationErrorBuffer.append(BpelsePluginMessages.getString("bpvisualizer_custom_y_axis"));
            validationErrorBuffer.append("<br/>");
            
        }
        
        return xValid && yValid;
        
    }
    
    private void validateNumericEntry(boolean testXVars) {
        String lVar = null;
        String uVar = null;
        String axis =  null;
        boolean isLowValueNumeric = true;
        boolean isHighValueNumeric = true;
        
        if(testXVars) {
            lVar = lowerLimit_x_axis;
            uVar = upperLimit_x_axis;
            axis = BpelsePluginMessages.getString("bpvisualizer_custom_x_axis");
        } else {
            lVar = lowerLimit_y_axis;
            uVar = upperLimit_y_axis;
            axis = BpelsePluginMessages.getString("bpvisualizer_custom_y_axis");
            
        }
        boolean lHasValue = lVar != null && lVar.length() != 0;
        if(lHasValue) {
            Double  dVal = DoubleValidator.getInstance().validate(lVar);
            Integer iVal = IntegerValidator.getInstance().validate(lVar);
            Float fVal = FloatValidator.getInstance().validate(lVar);
            Long lVal = LongValidator.getInstance().validate(lVar);
            if(dVal == null && iVal == null && fVal == null &&
                    lVal == null) {
                isLowValueNumeric = false;
                validationErrorBuffer.append(axis);
                validationErrorBuffer.append(BpelsePluginMessages.getString("bpvisualizer_custom_lowerlimit_label"));
                validationErrorBuffer.append(BpelsePluginMessages.getString("bpvisualizer_custom_no_numeric_var"));
                validationErrorBuffer.append("<br/>");
                hasValidEntries = false;
            }
        }
        boolean uHasValue = uVar != null && uVar.length() != 0;
        if(uHasValue) {
            Double  dVal = DoubleValidator.getInstance().validate(uVar);
            Integer iVal = IntegerValidator.getInstance().validate(uVar);
            Float fVal = FloatValidator.getInstance().validate(uVar);
            Long lVal = LongValidator.getInstance().validate(uVar);
            if(dVal == null && iVal == null && fVal == null &&
                    lVal == null) {
                
                isHighValueNumeric = false;
                validationErrorBuffer.append(axis);
                validationErrorBuffer.append(BpelsePluginMessages.getString("bpvisualizer_custom_upperlimit_label"));
                validationErrorBuffer.append(BpelsePluginMessages.getString("bpvisualizer_custom_no_numeric_var"));
                validationErrorBuffer.append("<br/>");
                hasValidEntries = false;
            }
        }
        
        if(lHasValue && uHasValue  && isHighValueNumeric && isLowValueNumeric) {
            // make sure upper limit value is high then lower limit value
            // convert all numeric types to double for the test
            if((DoubleValidator.getInstance().validate(lVar).doubleValue() -
                    DoubleValidator.getInstance().validate(uVar).doubleValue()) > 0) {
                
                validationErrorBuffer.append(axis);
                validationErrorBuffer.append(BpelsePluginMessages.getString("bpvisualizer_custom_u_is_smaller_l"));
                validationErrorBuffer.append("<br/>");
                hasValidEntries = false;
                
            }
        }
        
    }
    
    
    private void validateDateEntry(boolean testXVars) {
        Date lVar = null;
        Date uVar = null;
        String axis =  null;
        
        if(testXVars) {
            lVar = dateLowerLimit_x_axis;
            uVar = dateUpperLimit_x_axis;
            axis = BpelsePluginMessages.getString("bpvisualizer_custom_x_axis");
        } else {
            lVar = dateLowerLimit_y_axis;
            uVar = dateUpperLimit_y_axis;
            axis = BpelsePluginMessages.getString("bpvisualizer_custom_y_axis");
            
        }
        boolean lHasValue = lVar != null;
        if(lHasValue) {
            if(lVar == null) {
                
                validationErrorBuffer.append(axis);
                validationErrorBuffer.append(BpelsePluginMessages.getString("bpvisualizer_custom_lowerlimit_label"));
                validationErrorBuffer.append(BpelsePluginMessages.getString("bpvisualizer_custom_no_date_var"));
                validationErrorBuffer.append("<br/>");
                hasValidEntries = false;
            }
        }
        
        boolean uHasValue =uVar != null ;
        if(uHasValue) {
            if(uVar == null) {
                
                validationErrorBuffer.append(axis);
                validationErrorBuffer.append(BpelsePluginMessages.getString("bpvisualizer_custom_upperlimit_label"));
                validationErrorBuffer.append(BpelsePluginMessages.getString("bpvisualizer_custom_no_date_var"));
                validationErrorBuffer.append("<br/>");
                hasValidEntries = false;
            }
        }
        if(lHasValue && uHasValue) {
            // make sure upper limit value is high then lower limit value
            // convert all numeric types to double for the test
            int compareValue =DateValidator.getInstance().compareDates(uVar,
                    lVar,null);
            if(compareValue == -1) {
                
                validationErrorBuffer.append(axis);
                validationErrorBuffer.append(BpelsePluginMessages.getString("bpvisualizer_custom_u_is_smaller_l"));
                validationErrorBuffer.append("<br/>");
                hasValidEntries = false;
                
            }
        }
    }
    
    
    
    
    VAR_COMBINATION getCurrentVarTypeCombination() {
        VAR_COMBINATION combination = VAR_COMBINATION.NA;
        if(selectedXVariableType == VAR_TYPE.NUMERIC && 
                selectedYVariableType == VAR_TYPE.NUMERIC) {
            combination = VAR_COMBINATION.NUMERIC_NUMERIC;
        } else if((selectedXVariableType == VAR_TYPE.NUMERIC && 
                selectedYVariableType == VAR_TYPE.STRING) ||
                (selectedYVariableType == VAR_TYPE.NUMERIC && 
                 selectedXVariableType == VAR_TYPE.STRING))
                {
            combination = VAR_COMBINATION.NUMERIC_STRING;
        } else if((selectedXVariableType == VAR_TYPE.NUMERIC && 
                selectedYVariableType == VAR_TYPE.BOOLEAN) ||
                (selectedYVariableType == VAR_TYPE.NUMERIC && 
                 selectedXVariableType == VAR_TYPE.BOOLEAN))
                {
            combination = VAR_COMBINATION.NUMERIC_BOOLEAN;
        } else if((selectedXVariableType == VAR_TYPE.NUMERIC && 
                selectedYVariableType == VAR_TYPE.DATETIME) ||
                (selectedYVariableType == VAR_TYPE.NUMERIC && 
                 selectedXVariableType == VAR_TYPE.DATETIME))
                {
            combination = VAR_COMBINATION.NUMERIC_DATETIME;
        }   
        return combination;
    }
        
    private Map<String,String> cloneMap(Map<String,String> map) {
        Map<String,String> clonedMap= new HashMap<String,String>();
        for (String key : map.keySet()) {
            clonedMap.put(key, map.get(key));
        }
        return clonedMap;
    }
    
    
    String getNameForId(String id) {
        List mapsList = bpelProcessesMap.get(this.selectedBpelProcess);
        Map<String,String> id2VarMap = (Map<String,String>)mapsList.get(1);
        return id2VarMap.get(id);
    }
    
    String getidForName(String name) {
        List mapsList = bpelProcessesMap.get(this.selectedBpelProcess);
        Map<String,String> var2IdMap = (Map<String,String>)mapsList.get(0);
        return var2IdMap.get(name);
    }
   
    
    
    
}
