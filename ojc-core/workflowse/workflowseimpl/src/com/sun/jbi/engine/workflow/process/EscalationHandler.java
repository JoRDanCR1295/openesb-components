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
 * @(#)EscalationHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.process;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTaskTimer;
import com.sun.jbi.engine.workflow.runtime.model.TaskException;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.workflow.model.Assignment;
import com.sun.jbi.workflow.model.Escalation;
import com.sun.jbi.workflow.model.ModelException;

public class EscalationHandler implements Handler
{
    
    private static final Logger LOGGER = Logger.getLogger (EscalationHandler.class.getName ());
      
    private RuntimeTaskTimer mRTaskTimer;
    
    private Escalation mEscalation;
    
    private TaskManager mManager;
  
    private Date mRecoveredDueDate;
    
    public EscalationHandler (RuntimeTaskTimer rTaskTimer,  TaskManager manager)
    {
        this.mRTaskTimer = rTaskTimer;
        this.mEscalation = (Escalation) rTaskTimer.getTimerMetaData ();
        this.mManager = manager;
        this.mRecoveredDueDate = rTaskTimer.getDueDate();
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
                Date deadline = this.mEscalation.getDeadlineObject (runtimeTask.getJXpathContext());
                Date duration = this.mEscalation.getDurationDate (runtimeTask.getJXpathContext());

                if(deadline != null)
                {
                    processDeadline (deadline);
                }
                else if(duration != null)
                {
                    processDuration (duration);
                }
            }
        }
        catch(ModelException ex)
        {
            throw new TaskException (I18n.loc ("WLM-6049: Neither deadline nor duration is found in task defintion"), ex);
        }
        
    }
    
    private void processDeadline (Date deadline)
    {
//        Timer timer = new Timer ();
        TimerTask tt = new EscalationTimerTask ();
        long delay = deadline.getTime() - System.currentTimeMillis();
        ScheduledFuture<?>  timer = TaskHandlerManager.getInstance().getTimerPool().schedule(tt, delay, TimeUnit.MILLISECONDS);        
        this.mRTaskTimer.setTimerTask (timer);
//        timer.schedule (tt, deadline);
    }
    
    private void processDuration (Date durationDate)
    {
//        Timer timer = new Timer ();
        TimerTask tt = new EscalationTimerTask ();
        long delay = durationDate.getTime() - System.currentTimeMillis();
        ScheduledFuture<?>  timer = TaskHandlerManager.getInstance().getTimerPool().schedule(tt, delay, TimeUnit.MILLISECONDS);        
        this.mRTaskTimer.setTimerTask (timer);
//        timer.schedule (tt, durationDate);
    }
    
    class EscalationTimerTask extends TimerTask
    {
        @Override
        public void run ()
        {
            try
            {
                RuntimeTask runtimeTask =  mRTaskTimer.getTask();
                Assignment a = mEscalation.getAssignment (runtimeTask.getJXpathContext());
                if(a != null)
                {
                        List<TaskPrincipal> tPrincipals = new ArrayList<TaskPrincipal>();
                        List<TaskPrincipal> tExcludedPrincipals = new ArrayList<TaskPrincipal>();
                        //users
                        List<String> users = a.getUsers (runtimeTask.getJXpathContext());
                        if(users != null)
                        {
                            Iterator<String> it = users.iterator ();
                            while(it.hasNext ())
                            {
                                String user = it.next ();
                                TaskPrincipal tp = TaskModelFactory.getInstance ().createPrincipal (user, TaskPrincipal.PrincipalType.User);
                                tPrincipals.add (tp);
                            }                        
                        
                        //groups
                        List<String> groups = a.getGroups (runtimeTask.getJXpathContext());
                        if(groups != null)
                        {
                            it = groups.iterator ();
                            while(it.hasNext ())
                            {
                                String group = it.next ();
                                TaskPrincipal tp = TaskModelFactory.getInstance ().createPrincipal (group, TaskPrincipal.PrincipalType.Group);
                                tPrincipals.add (tp);
                                
                            }
                        }
                        
                        //Excluded users
                        List<String> excludedUsers = a.getExcludedUsers(runtimeTask.getJXpathContext());
                        if(excludedUsers != null)
                        {
                             it = excludedUsers.iterator ();
                            while(it.hasNext ())
                            {
                                String user = it.next ();
                                TaskPrincipal tp = TaskModelFactory.getInstance ().createPrincipal (user, TaskPrincipal.PrincipalType.User);
                                tExcludedPrincipals.add (tp);
                            } 
                        }
                        
                        //Excluded groups
                        List<String> excludedGroups = a.getExcludedGroups(runtimeTask.getJXpathContext());
                        if(excludedGroups != null)
                        {
                            it = excludedGroups.iterator ();
                            while(it.hasNext ())
                            {
                                String group = it.next ();
                                TaskPrincipal tp = TaskModelFactory.getInstance ().createPrincipal (group, TaskPrincipal.PrincipalType.Group);
                                tExcludedPrincipals.add (tp);                                
                            }
                        }                        
                         mManager.escalateTask (mEscalation, runtimeTask.getId (), mRTaskTimer.getId (),  tPrincipals, tExcludedPrincipals);
                    }
                }
            }
            catch(Exception ex)
            {
                LOGGER.log (Level.WARNING, I18n.loc ("WLM-6080: Failed to process escalation logic"), ex);
            }
            
        }
    }
    
    
}
