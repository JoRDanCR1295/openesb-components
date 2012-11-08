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
 * @(#)ChartingConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.bpvisualizer;

import com.sun.jbi.cam.common.resources.Messages;

/**
 *
 * @author rdamir
 */
public class ChartingConstants {
    
  public final static String BPS_AGGREGATED_STATUS = "aggregated-status";
  public final static String BP_SERVICE_UNIT_STATUS = "bpel-su-status"; 
  public final static String BP_SERVICE_UNIT_CUSTOM = "bpel-su-custom"; 
  public static String SELECT_CHART_STRING;
  protected static String BPS_AGGREGATED_STATUS_DISPLAY;
  protected static String BP_SERVICE_UNIT_STATUS_DISPLAY;
  protected static String BP_SERVICE_UNIT_CUSTOM_DISPLAY;
  protected static String SELECT_VAR;

  
  static {
      BPS_AGGREGATED_STATUS_DISPLAY =
              Messages.getString("bpvisualizer_Bpel_Aggregated_status");
      BP_SERVICE_UNIT_STATUS_DISPLAY =
              Messages.getString("bpvisualizer_Bpel_su_status");
      BP_SERVICE_UNIT_CUSTOM_DISPLAY =
              Messages.getString("bpvisualizer_Bpel_su_custom");
      SELECT_VAR =
              Messages.getString("bpvisualizer_custom_select_var");
      SELECT_CHART_STRING  = 
        Messages.getString("bpvisualizer_Bpel_select_chart");
      
      
  }
    
}
