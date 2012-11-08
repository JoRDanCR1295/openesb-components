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
 * @(#)BPVisualizerDBHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.bpvisualizer;

import com.sun.jbi.cam.manager.framework.common.ChartType;
import com.sun.jbi.cam.plugins.bpelse.DBManager;
import java.io.InputStream;
import java.sql.Blob;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.jfree.data.category.DefaultCategoryDataset;
import org.jfree.data.general.Dataset;
import org.jfree.data.general.DefaultPieDataset;
import org.jfree.data.xy.DefaultXYDataset;

/**
 *
 * @author rdamir
 */
public class BPVisualizerDBHelper {
    
    private Logger logger = Logger.getLogger(BPVisualizerDBHelper.class.getName());
    private DBManager dbManager;
    
    
    
    public BPVisualizerDBHelper() {
        dbManager =  new DBManager();
    }

    public void setDBManager( DBManager dbManager) {
        this.dbManager =  dbManager;
    }

    
    
    /**
     * query the db for the data to be used to generated the dataset based on the 
     * selected chart type
     * @param customQueryData - instance of the custom chart query data.
     * @return - SQL ResultSet containning the data returned from the db.
     * @throws SQLException - if failed to execute the query successfully
     *           could be empty if no data returned.   
     */
    public ResultSet getCustomDataSet(BPCustomChartQuery customQueryData) throws SQLException{
        String query = customQueryData.getCustomQueryString();
        logger.fine(query);
        ResultSet resultset = dbManager.execGenericQuery(query);
        return resultset;
    }

    
}
