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
 * @(#)TimeOutHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.process;

import java.io.StringReader;
import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;

import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTaskTimer;
import com.sun.jbi.engine.workflow.runtime.model.TaskException;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.util.Util;
import com.sun.jbi.workflow.model.ModelException;
import com.sun.jbi.workflow.model.Timeout;

public class TimeOutHandler implements Handler
{
    
    private Logger mLogger = Logger.getLogger (TimeOutHandler.class.getName ());
    
    private RuntimeTaskTimer mRTaskTimer;
    
    private Timeout mTimeout;
    
    private TaskHandlerManager mHanderManager;
    
    private TaskManager mManager;
    
    private Date mRecoveredDueDate;
    
    public TimeOutHandler (RuntimeTaskTimer rTaskTimer,
        TaskHandlerManager handerManager,
        TaskManager manager)
    {
        this.mRTaskTimer = rTaskTimer;
        this.mTimeout = (Timeout) rTaskTimer.getTimerMetaData ();
        this.mHanderManager = handerManager;
        this.mManager = manager;
        this.mRecoveredDueDate = rTaskTimer.getDueDate ();
    }

    public void execute () throws TaskException
    {
        try
        {
            if(mRecoveredDueDate != null)
            {
                processDeadline(mRecoveredDueDate);
            }
            else
            {
                RuntimeTask runtimeTask =  mRTaskTimer.getTask();
                Date deadline = this.mTimeout.getDeadlineObject (runtimeTask.getJXpathContext());
                Date durationDate = this.mTimeout.getDurationDate (runtimeTask.getJXpathContext());
                
                if(deadline != null)
                {
                    processDeadline (deadline);
                }
                else if(durationDate != null)
                {
                    processDuration (durationDate);
                }
            }
        }
        catch(ModelException ex)
        {
            throw new TaskException ("Neither deadline nor duration is found in task defintion", ex);
        }
        
    }
    
    private void processDeadline (Date deadline)
    {
//        Timer timer = new Timer ();
        TimerTask tt = new TimeoutTimerTask ();
        long delay = deadline.getTime() - System.currentTimeMillis();
        ScheduledFuture<?>  timer = mHanderManager.getTimerPool().schedule(tt, delay, TimeUnit.MILLISECONDS);
        this.mRTaskTimer.setTimerTask (timer);
        
//        timer.schedule (tt, deadline);
    }
    
    private void processDuration (Date durationDate)
    {
//        Timer timer = new Timer ();
        TimerTask tt = new TimeoutTimerTask ();
        long delay = durationDate.getTime() - System.currentTimeMillis();
        ScheduledFuture<?>  timer = mHanderManager.getTimerPool().schedule(tt, delay, TimeUnit.MILLISECONDS);
        
        this.mRTaskTimer.setTimerTask (timer);
//        timer.schedule (tt, durationDate);
    }
    
    class TimeoutTimerTask extends TimerTask
    {
        @Override
        public void run ()
        {
            try
            {
                Element tFault  = null;
                try {
                     tFault = Util.makeTimeoutFault ();
                }catch (Exception e) {
                    throw new TaskException (e);
                }
                RuntimeTask runtimeTask =  mRTaskTimer.getTask();
                mManager.timeoutTask (runtimeTask.getId (), mRTaskTimer.getId ());
                mHanderManager.notifyOnTimeout (runtimeTask.getId (), tFault, runtimeTask.getExchangeId ());
            }
            catch(TaskException ex)
            {
                mLogger.log (Level.SEVERE, ex.getMessage ());
            }
        }
    }
    
//    private Element makeTimeoutFault ()
//    {
//        try
//        {
//            //use hardcoded system fault format for task timeout
//            // This is ok to hardcode since we have only one system fault as of today
//            String sysTimeoutFault = "<SystemFault version=\"1.0\" xmlns=\"http://java.sun.com/xml/ns/jbi/systemfaults\" >" +
//            						 "<code>receiver</code>" +
//            						 "<subcode>Timed Out</subcode>" +
//            						 "<reason>This task is not completed within specified time and timed out </reason>" +
//            						 "</SystemFault>";
//            
//            InputSource in = new InputSource (new StringReader (sysTimeoutFault));
//            
//            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance ();
//            factory.setNamespaceAware (true);
//            Document doc = factory.newDocumentBuilder ().parse (in);
//            
//            
//            Element timeoutEl = doc.getDocumentElement ();
//            return timeoutEl;
//            /*Map<String, Element> partsMap = new HashMap<String, Element>();
//            partsMap.put ("fault", timeoutEl);
//            return JBIMessageUtil.makeJBIMessage (partsMap, this.mRTaskTimer.getTask ().getTaskMeta ().getWSDLOperation ());
//            */
//        }
//        catch (Exception ex)
//        {
//            mLogger.log (Level.SEVERE, ex.getMessage ());
//        }
        
//        return null;
//    }
    
    
    
}
