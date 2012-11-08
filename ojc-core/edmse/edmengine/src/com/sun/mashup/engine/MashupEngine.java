package com.sun.mashup.engine;

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

//package com.sun.etl.engine;

import java.util.List;
import java.util.Map;
import javax.sql.rowset.WebRowSet;
import java.sql.ResultSet;

import org.w3c.dom.Element;

import com.sun.sql.framework.exception.BaseException;
import java.sql.SQLException;

/**
 * This interface contains all the methods that will be implemented by MashupEngineImpl
 * 
 * @author Srinivasan Rengarajan
 * @version :$Revision: 1.3 $
 */
public interface MashupEngine {

    /**
     * execute the tasks by invoking process() method.
     * 
     * @param execListener Which implements ETLEngineExecListener
     * @return an integer
     */
    public ResultSet exec()throws Exception;

 
    /**
     * get the mashup process definition object associated
     * with this engine instance
     */
    public EDMProcessDefinition getMashupDefinition();
    
    /**
     * set the mashup process definition object associated 
     * with this engine instance
     **/
    public void setMashupDefinition(EDMProcessDefinition def);

    /**
     * get display name of the engine.
     */
    public String getDisplayName();

   
    /**
     * set display name of the engine.
     */
    public void setDisplayName(String theDisplayName);

    /**
     * Starts ETLEngine
     */
    public void start();

    /**
     * Stop the MashupEngine using taskmanager
     */
    public void stopMashupEngine();

}

