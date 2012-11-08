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
 * @(#)BPCustomChartQuery.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.bpvisualizer;

/*
 * the class is used to collect  all the possible parameters needed to build
 * custom chart query. 
 */
public class BPCustomChartQuery {


 
    
    public String getXLowValue() {
        return xLowValue;
    }
    
    public void setXLowValue(String lowValue) {
        xLowValue = lowValue;
        if(xLowValue != null && xLowValue.length() > 0) {
            x_Limit_type = LIMIT_TYPE.LOW_LIMIT_ONLY;
            if(xHighValue != null && xHighValue.length() > 0) {
                x_Limit_type = LIMIT_TYPE.BOTH_LIMITS;
            }
        }
    }

    public String getXHighValue() {
        return xHighValue;
    }
    
    public void setXHighValue(String highValue) {
        xHighValue = highValue;
        if(xHighValue != null && xHighValue.length() > 0) {
            x_Limit_type = LIMIT_TYPE.HIGH_LIMIT_ONLY;
            if(xLowValue != null && xLowValue.length() > 0) {
                x_Limit_type = LIMIT_TYPE.BOTH_LIMITS;
            }
        }
    }

    public String getYLowValue() {
        return yLowValue;
    }
    
    public void setYLowValue(String lowValue) {
        yLowValue = lowValue;
        if(yLowValue != null && yLowValue.length() > 0) {
            y_Limit_type = LIMIT_TYPE.LOW_LIMIT_ONLY;
            if(yHighValue != null && yHighValue.length() > 0) {
                y_Limit_type = LIMIT_TYPE.BOTH_LIMITS;
            }
        }
    }
    
    public String getYHighValue() {
        return yHighValue;
    }

    public void setYHighValue(String highValue) {
        yHighValue = highValue;
        if(yHighValue != null && yHighValue.length() > 0) {
            y_Limit_type = LIMIT_TYPE.HIGH_LIMIT_ONLY;
            if(yLowValue != null && yLowValue.length() > 0) {
                y_Limit_type = LIMIT_TYPE.BOTH_LIMITS;
            }
        }
    }
    
    public void setXID(String xid) {
        int delimiterIndex = xid.lastIndexOf(".");
        bpelXId = xid.substring(0,delimiterIndex);
        varXId = xid.substring(delimiterIndex+1);
    }
    
    
    public BPCustomChartBean.VAR_TYPE getXType() {
        return xType;
    }
    
    public void setXType(BPCustomChartBean.VAR_TYPE type) {
        xType = type;
    }
    
    public String getYID() {
        return yID;
    }
    
    public void setYID(String yid) {
        int delimiterIndex = yid.lastIndexOf(".");
        bpelYId = yid.substring(0,delimiterIndex);
        varYId = yid.substring(delimiterIndex+1);
    }
    
    public BPCustomChartBean.VAR_TYPE getYType() {
        return yType;
    }
    
    public void setYType(BPCustomChartBean.VAR_TYPE type) {
        yType = type;
    }
    
    public String getBpelXId() {
        return bpelXId;
    }

    public String getBpelYId() {
        return bpelYId;
    }
    
    
    public String getVarXId() {
        return varXId;
    }

    public String getVarYId() {
        return varYId;
    }
    
    public String getCustomQueryString() {
        
        String customQuery = CUSTOM_DATA_QUERY;
        customQuery = customQuery.replaceAll(X_COLUMN_NAME, mapType2ColumnName(getXType()));
        customQuery = customQuery.replaceAll(Y_COLUMN_NAME, 
                mapType2ColumnName(getYType()));
        customQuery = customQuery.replaceAll(X_ID,getVarXId()); 
        customQuery = customQuery.replaceAll(Y_ID,getVarYId()); 
        
        if(getBpelXId().equals(getBpelYId())) {
            customQuery = customQuery.replaceAll(BPEL_IDS,"'"+ 
                    getBpelXId()+"'");
        }else {
            customQuery = customQuery.replaceAll(BPEL_IDS,"'"+ 
                    getBpelXId() + "','" + getBpelYId()+"'");
        }

        if(this.x_Limit_type != LIMIT_TYPE.NA) {
            customQuery += addLimits(x_Limit_type,true);
        }
        if(this.y_Limit_type != LIMIT_TYPE.NA) {
            customQuery += addLimits(y_Limit_type,false);
        }
       
        customQuery += ORDER_QUERY;
        
        return customQuery.toString();

    }
    
    private String addLimits(LIMIT_TYPE limitType, boolean isXLimit) {
        String filter = null;
        String value1 = null;
        String value2 = null;
        
        if(isXLimit) {
            switch (limitType) {
                case LOW_LIMIT_ONLY:
                    filter = FILTER_LOW_LIMIT;
                    if(getXType() == BPCustomChartBean.VAR_TYPE.NUMERIC) {
                        value1 = xLowValue; 
                    } else {
                        value1 = "'" + xLowValue + "'";    
                    }
                    break;
                case HIGH_LIMIT_ONLY:
                    filter = FILTER_HIGH_LIMIT;
                    if(getXType() == BPCustomChartBean.VAR_TYPE.NUMERIC) {
                        value1 = xHighValue; 
                    } else {
                        value1 = "'" + xHighValue + "'";    
                    }
                     break;
                case BOTH_LIMITS:
                    filter = FILTER_BOTH_LIMIT;
                    if(getXType() == BPCustomChartBean.VAR_TYPE.NUMERIC) {
                        value1 = xLowValue; 
                        value2 = xHighValue; 
                    } else {
                        value1 = "'" + xLowValue + "'";    
                        value2 = "'" + xHighValue + "'";    
                    }
                 break;
           }
           filter = DoFilterReplacement(filter, mapType2ColumnName(getXType()),
                    mapType2ColumnName(getYType()),value1,value2);
        } else {
            switch (limitType) {
                case LOW_LIMIT_ONLY:
                    filter = FILTER_LOW_LIMIT;
                    if(getYType() == BPCustomChartBean.VAR_TYPE.NUMERIC) {
                        value1 = yLowValue; 
                    } else {
                        value1 = "'" + yLowValue + "'";    
                    }
                     break;
                case HIGH_LIMIT_ONLY:
                    filter = FILTER_HIGH_LIMIT;
                    if(getYType() == BPCustomChartBean.VAR_TYPE.NUMERIC) {
                        value1 = yHighValue; 
                    } else {
                        value1 = "'" + yHighValue + "'";    
                    }
                    break;
                case BOTH_LIMITS:
                    filter = FILTER_BOTH_LIMIT;
                    if(getYType() == BPCustomChartBean.VAR_TYPE.NUMERIC) {
                        value1 = yLowValue; 
                        value2 = yHighValue; 
                    } else {
                        value1 = "'" + yLowValue + "'";    
                        value2 = "'" + yHighValue + "'";    
                    }
                 break;
            }
            filter = DoFilterReplacement(filter,mapType2ColumnName(getYType()),
                    mapType2ColumnName(getXType()),value1,value2);
        }
        
        return filter;
    }
    
    private String DoFilterReplacement(String filter,String firstColName,
            String secondColName, String firstValue, String secondValue) {
        String fixedFilter = filter;
        fixedFilter = fixedFilter.replaceAll(FIRST_COLUMN_NAME, firstColName);
        fixedFilter = fixedFilter.replaceAll(SECOND_COLUMN_NAME, secondColName);
        if(secondValue == null) {
            fixedFilter = fixedFilter.replaceAll(COLUMN_VALUE, firstValue);
        } else {
            fixedFilter = fixedFilter.replaceAll(COLUMN_VALUE_LOW, firstValue);
            fixedFilter = fixedFilter.replaceAll(COLUMN_VALUE_HIGH, secondValue);
           
        }
        return fixedFilter;
    }
    
    private String mapType2ColumnName(BPCustomChartBean.VAR_TYPE type) {
        switch (type) {
        case BOOLEAN:
            return BOOLEAN_VALUE; 
        case DATETIME:
            return DATETIME_VALUE; 
        case STRING:
            return STRING_VALUE; 
        case NUMERIC:
            return NUMERIC_VALUE; 
        }
        return null;
    }

    
    private enum LIMIT_TYPE { LOW_LIMIT_ONLY, HIGH_LIMIT_ONLY ,BOTH_LIMITS,NA};
    private BPCustomChartBean.VAR_TYPE xType;
    private BPCustomChartBean.VAR_TYPE yType;
//    private String xID; 
    private String yID;
    private String xLowValue;
    private String xHighValue;
    private String yLowValue;
    private String yHighValue;
    private String varXId;
    private String varYId;
    private String bpelXId;
    private String bpelYId;
    private LIMIT_TYPE x_Limit_type = LIMIT_TYPE.NA;
    private LIMIT_TYPE y_Limit_type = LIMIT_TYPE.NA;

    private final static String BOOLEAN_VALUE = "BOOLEANVALUE";
    private final static String NUMERIC_VALUE = "NUMERICVALUE";
    private final static String DATETIME_VALUE = "DATETIMEVALUE";
    private final static String STRING_VALUE = "STRINGVALUE";
    private final static String X_COLUMN_NAME = "~VAR_X_COLUMN~";
    private final static String Y_COLUMN_NAME = "~VAR_Y_COLUMN~";
    private final static String FIRST_COLUMN_NAME = "~FIRST_COLUMN_NAME~";
    private final static String SECOND_COLUMN_NAME = "~SECOND_COLUMN_NAME~";
    private final static String COLUMN_VALUE = "~COLUMN_VALUE~";
    private final static String COLUMN_VALUE_LOW = "~VAR_X_VALUE_LOW~";
    private final static String COLUMN_VALUE_HIGH = "~VAR_X_VALUE_HIGH~";
    private final static String X_ID = "~VARX_ID~";
    private final static String Y_ID = "~VARY_ID~";
    private final static String BPEL_IDS = "~BPEL_ID~";
    private final static String ORDER_QUERY = " ORDER BY STATEID,VARID"; 
    private final static String STATEID_QUERY = 
       " SELECT STATEID FROM STATE WHERE BPELID IN (" +
       BPEL_IDS + ")";
    private final static String FILTER_LOW_LIMIT =" AND " +
           "(" + FIRST_COLUMN_NAME + ">=" + COLUMN_VALUE + " or " +
           "(" + FIRST_COLUMN_NAME + " is null and " + SECOND_COLUMN_NAME + 
           " is not null))";
    private final static String FILTER_HIGH_LIMIT =" AND " +
            "(" + FIRST_COLUMN_NAME + "<=" + COLUMN_VALUE + " or " +
            "(" + FIRST_COLUMN_NAME + " is null and " + SECOND_COLUMN_NAME + 
            " is not null))";
    private final static String FILTER_BOTH_LIMIT =" and  " +
            "( " + FIRST_COLUMN_NAME + " >= " +  COLUMN_VALUE_LOW + " and  " +
            FIRST_COLUMN_NAME + " <= "  + COLUMN_VALUE_HIGH +  " or ( " + 
            FIRST_COLUMN_NAME + " is null and " + SECOND_COLUMN_NAME + " is not null))";
 
    
    private final static String CUSTOM_DATA_QUERY = 
       "SELECT STATEID,VARID," +  X_COLUMN_NAME +"," + Y_COLUMN_NAME + 
       " from SIMPLEVARIABLE WHERE EXISTS (" + STATEID_QUERY + 
       ") AND  VARID IN ( " + X_ID +"," + Y_ID +") ";



    
}
