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
 * Copyright 2007-2008 ZAZ Consulting, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.zaz.ssapi.protocol.common.model;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 *
 * @author liyunhai
 */
public class CustomizeOutputInfo implements Serializable {

    private Map<Integer, List<CustomizeOutputField>> outputInfo =
            new HashMap<Integer, List<CustomizeOutputField>>();

    public Map<Integer, List<CustomizeOutputField>> getOutputInfo() {
        return outputInfo;
    }

    public void addCustomizeOutputFields(int index, List<CustomizeOutputField> fields) {
        outputInfo.put(Integer.valueOf(index), fields);
    }

    public void addTopCustomizeOutputFields(List<CustomizeOutputField> fields) {
        int index = outputInfo.size();
        outputInfo.put(Integer.valueOf(index), fields);
    }

    public void removeTopCustomizeOutputFields() {
        if (outputInfo.size() > 0) {
            outputInfo.remove(Integer.valueOf(outputInfo.size() - 1));
        }
    }

    public List<CustomizeOutputField> getCustomizeOutputFields(int index) {
        return outputInfo.get(Integer.valueOf(index));
    }

    public List<CustomizeOutputField> getTopCustomizeOutputFields() {
        int index = outputInfo.size() - 1;
        return outputInfo.get(Integer.valueOf(index));
    }

    public void clear() {
        outputInfo.clear();
    }
}
