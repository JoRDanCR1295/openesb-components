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
 * @(#)BPELProcessMaps.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.bpvisualizer;

import java.util.List;
import java.util.Map;

import com.sun.jbi.cam.plugins.bpelse.bpvisualizer.BPCustomChartBean.VAR_TYPE;

public class BPELProcessMaps {

    
    Map<String,List<Map<String,String>>> bpelProcessesMap;
    Map<String,Map<String,VAR_TYPE>> bpelName2id2TypeMap;
    
    
  public Map<String, Map<String, VAR_TYPE>> getBpelName2id2TypeMap() {
      return bpelName2id2TypeMap;
  }
  public void setBpelName2id2TypeMap(
          Map<String, Map<String, VAR_TYPE>> bpelName2id2TypeMap) {
      this.bpelName2id2TypeMap = bpelName2id2TypeMap;
  }
  public Map<String, List<Map<String, String>>> getBpelProcessesMap() {
      return bpelProcessesMap;
  }
  public void setBpelProcessesMap(
          Map<String, List<Map<String, String>>> bpelProcessesMap) {
      this.bpelProcessesMap = bpelProcessesMap;
  }
    
    
    
}
