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
 * @(#)Sheperd.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.client.pojo;

import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;
import java.util.Map;

/**
 * Sheperd.java
 *
 * Created on September 7, 2005, 11:53 PM
 *
 * @author Bing Lu
 */

public interface Sheperd extends OperatorConstants {
    public static final String SHEPERD_NAME = "sheperdName";
    public static final String SHEPERD_INSTANCE_ID = "instanceId";
    public static final String SHEPERD_OP_ID = "opId";
    public static final String SHEPERD_OP_NAME = "opName";
    public static final String SHEPERD_REPEAT = "repeat";
    public static final String SHEPERD_BATCH_SIZE = "batchSize";
    public static final String SHEPERD_COLUMN_NAMES = "columnNames";
    public static final String SHEPERD_ROW_LIST = "rowList";

    public void init(Map prop);
    public void begin() throws Exception;
    public int input(int index) throws Exception;
    public void end();
}
