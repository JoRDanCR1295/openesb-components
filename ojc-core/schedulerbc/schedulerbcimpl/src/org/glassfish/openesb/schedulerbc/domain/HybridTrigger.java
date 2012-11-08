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
 * @(#)HybridTrigger.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain;

import java.text.ParseException;
import java.util.Date;
import java.util.TimeZone;
import java.util.logging.Logger;
import org.glassfish.openesb.schedulerbc.I18n;
import org.quartz.Calendar;
import org.quartz.CronExpression;
import org.quartz.CronTrigger;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;

/**
 * A hybrid implementation comprising a Quartz <code>CronTrigger</code>
 * essentially triggering a Quartz <code>SimpleTrigger</code>.
 * 
 * @author sunsoabi_edwong
 */
public class HybridTrigger extends Trigger implements SchedulerConstants {

    private long duration;
    private int repeatCount;
    private long repeatInterval;
    private boolean cronToBeTriggered = false;
    private boolean cronJustTriggered = false;
    private boolean simpleToBeTriggered = false;
    private boolean simpleJustTriggered = false;
    private long cronLastTriggered = 0L;
    CronTriggerProxy cronDelegate = null;
    SimpleTriggerProxy simpleDelegate = null;
    
    private transient Calendar quartzCal = null;
    private transient Logger logger = null;
    private transient CronExpression cronExpressionHelper = null;
    transient java.util.Calendar calTool = null;
    
    private static final String CRON_TRIG_SUFFIX = "-HybridCron";       //NOI18N
    private static final String SIMPLE_TRIG_SUFFIX = "-HybridSimple";   //NOI18N
    private static final String NULINE =
            System.getProperty("line.separator");                       //NOI18N
    
    // Makeup for missed fire times
    /**
     * <p>
     * Instructs the <code>{@link Scheduler}</code> that upon a mis-fire
     * situation, the <code>{@link HybridTrigger}</code> wants to be
     * re-scheduled to 'now' (even if the associated
     * <code>{@link Calendar}</code> excludes 'now') with the repeat count
     * left as-is.
     * </p>
     */
    public static final int MISFIRE_INSTRUCTION_RESCHEDULE_NOW_WITH_EXISTING_REPEAT_COUNT =
            SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NOW_WITH_EXISTING_REPEAT_COUNT;
    
    // No makeup for missed fire times
    /**
     * <p>
     * Instructs the <code>{@link Scheduler}</code> that upon a mis-fire
     * situation, the <code>{@link HybridTrigger}</code> wants to be
     * re-scheduled to 'now' (even if the associated
     * <code>{@link Calendar}</code> excludes 'now') with the repeat count
     * set to what it would be, if it had not missed any firings.
     * </p>
     */
    public static final int MISFIRE_INSTRUCTION_RESCHEDULE_NOW_WITH_REMAINING_COUNT =
            SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NOW_WITH_REMAINING_REPEAT_COUNT;
    
    /**
     * <p>
     * Instructs the <code>{@link Scheduler}</code> that upon a mis-fire
     * situation, the <code>{@link HybridTrigger}</code> wants to be
     * re-scheduled to the next scheduled time after 'now' - taking into
     * account any associated <code>{@link Calendar}</code>, and with the
     * repeat count set to what it would be, if it had not missed any firings.
     * </p>
     */
    public static final int MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_EXISTING_COUNT =
            SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_EXISTING_COUNT;

    /**
     * <p>
     * Instructs the <code>{@link Scheduler}</code> that upon a mis-fire
     * situation, the <code>{@link HybridTrigger}</code> wants to be
     * re-scheduled to the next scheduled time after 'now' - taking into
     * account any associated <code>{@link Calendar}</code>, and with the
     * repeat count left unchanged.
     * </p>
     */
    public static final int MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_REMAINING_COUNT =
            SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_REMAINING_COUNT;

    public HybridTrigger(String name, String group,
            Date startTime, Date endTime, String cronExpression,
            TimeZone timeZone, long duration, int repeatCount,
            long repeatInterval, Logger logger)
                    throws ParseException {
        super(name, group);
        
        cronDelegate = new CronTriggerProxy(name + CRON_TRIG_SUFFIX,
                group + CRON_TRIG_SUFFIX, cronExpression);
        cronDelegate.setStartTime(startTime);
        cronDelegate.setEndTime(endTime);
        cronDelegate.setTimeZone(timeZone);
        
        this.duration = duration;
        this.repeatCount = repeatCount;
        this.repeatInterval = repeatInterval;
        this.logger = logger;
        
        simpleDelegate = new SimpleTriggerProxy(name + SIMPLE_TRIG_SUFFIX,
                getGroup() + SIMPLE_TRIG_SUFFIX);
        Date never = new Date(Long.MAX_VALUE - 1000L);
        simpleDelegate.setStartTime(never);
        simpleDelegate.setNextFireTime(never);
        simpleDelegate.setRepeatCount(repeatCount);
        simpleDelegate.setRepeatInterval(repeatInterval);
        
        setMisfireInstruction(MISFIRE_INSTRUCTION_SMART_POLICY);
    }

    @Override
    public synchronized Object clone() {
        HybridTrigger copy = (HybridTrigger) super.clone();
        copy.cronDelegate = (CronTriggerProxy) cronDelegate.clone();
        copy.simpleDelegate = (SimpleTriggerProxy) simpleDelegate.clone();
        copy.calTool = (java.util.Calendar) getCalTool().clone();
        
        if (I18n.finestLoggable(log())) {
            I18n.finest(log(), "SCHEDBC-1028: {0} called HybridTrigger" //NOI18N
                    + "#clone()-->{1}",                                 //NOI18N
                    whoWhen(), toString(copy));
        }
        return copy;
    }
    
    public String getCronExpression() {
        return cronDelegate.getCronExpression();
    }
    
    private CronExpression getCronExpressionHelper() {
        if (null == cronExpressionHelper) {
            try {
                cronExpressionHelper =
                        new CronExpression(getCronExpression());
                cronExpressionHelper.setTimeZone(getTimeZone());
            } catch (ParseException pe) {
                // ignore since it passed already in the constructor above
            }
        }
        return cronExpressionHelper;
    }
    
    public long getDuration() {
        return duration;
    }
    
    public int getRepeatCount() {
        return repeatCount;
    }
    
    public long getRepeatInterval() {
        return repeatInterval;
    }
    
    private Logger log() {
        if (null == logger) {
            logger = Logger.getLogger(getClass().getName());
        }
        return logger;
    }
    
    private String toString(Object obj) {
        return "HybridTrigger@" + Integer.toHexString(                  //NOI18N
                        System.identityHashCode(obj)).toUpperCase();
    }
    
    private String whoWhen() {
        return toString(this)
                + ", " + XSD_STD_SDF.format(new Date()) + ":";          //NOI18N
    }
    
    private String timeStr(Date time) {
        if (time != null) {
            return XSD_STD_SDF.format(time.getTime());
        }
        return "never";                                                 //NOI18N
    }
    
    private String repeatStr(int repeat) {
        if (SimpleTrigger.REPEAT_INDEFINITELY == repeat) {
            return "indefinite";                                        //NOI18N
        }
        return Integer.toString(repeat);
    }
    
    protected boolean before(Date d1, Date d2) {
        return ((d1 != null) && (d2 != null)) ? d1.before(d2) : false;
    }
    
    protected boolean after(Date d1, Date d2) {
        return ((d1 != null) && (d2 != null)) ? d1.after(d2) : false;
    }
    
    protected boolean equals(Date d1, Date d2) {
        return ((d1 != null) && (d2 != null)) ? d1.equals(d2) : d1 == d2;
    }

    protected java.util.Calendar getCalTool() {
        if (null == calTool) {
            calTool = java.util.Calendar.getInstance(
                    cronDelegate.getTimeZone());
        }
        return calTool;
    }
    
    @Override
    public void triggered(Calendar calendar) {
        quartzCal = calendar;
        cronJustTriggered = false;
        simpleJustTriggered = false;
        
        if (cronToBeTriggered) {
            cronLastTriggered = cronDelegate.getNextFireTime().getTime();
            cronDelegate.triggered(calendar);
            cronToBeTriggered = false;
            cronJustTriggered = true;
            if (I18n.finerLoggable(log())) {
                I18n.finer(log(), "SCHEDBC-2004: {0} CronTrigger "      //NOI18N
                        + "delegate triggered(): start new "            //NOI18N
                        + "SimpleTrigger period", whoWhen());           //NOI18N
            }
            
            restartSimpleTrigger(cronDelegate.getPreviousFireTime(), calendar);
            simpleToBeTriggered = true;
            
            if (I18n.finestLoggable(log())) {
                I18n.finest(log(), "SCHEDBC-1021: {0} SimpleTrigger "   //NOI18N
                        + "delegate will trigger next",                 //NOI18N
                        whoWhen());
            }
        }
        
        if (simpleToBeTriggered) {
            simpleDelegate.triggered(calendar);
            simpleToBeTriggered = false;
            simpleJustTriggered = true;
            
            if (I18n.finestLoggable(log())) {
                I18n.finest(log(), "SCHEDBC-1017: {0} SimpleTrigger "   //NOI18N
                        + "delegate triggered()", whoWhen());           //NOI18N
            }
        }
        
        whoToTriggerNext();
    }

    private void whoToTriggerNext() {
        cronToBeTriggered = false;
        simpleToBeTriggered = false;
        Date cronNext = cronDelegate.getNextFireTime();
        Date simpleNext = simpleDelegate.getNextFireTime();
        
        if ((cronNext != null) && (equals(cronNext, simpleNext)
                || (null == simpleNext) || before(cronNext, simpleNext))) {
            cronToBeTriggered = true;
            
            if (I18n.finestLoggable(log())) {
                I18n.finest(log(), "SCHEDBC-1020: {0} CronTrigger "     //NOI18N
                        + "delegate will trigger next at {1}",          //NOI18N
                        whoWhen(), timeStr(cronNext));
            }
        } else if (simpleNext != null) {
            simpleToBeTriggered = true;
            
            if (I18n.finestLoggable(log())) {
                I18n.finest(log(), "SCHEDBC-1021: {0} SimpleTrigger "   //NOI18N
                        + "delegate will trigger next at {1}",          //NOI18N
                        whoWhen(), timeStr(simpleNext));
            }
        }
    }
    
    protected Date calculateEndTime(boolean inclusive, Date startTime) {
        long effDuration = duration;
        if (inclusive && (duration % repeatInterval) == 0) {
            effDuration += (repeatInterval / 2L);  // to include end boundary
        }
        getCalTool().setTime(startTime);
        getCalTool().add(java.util.Calendar.MILLISECOND, (int) effDuration);
        Date endTime = getCalTool().getTime();
        if ((getEndTime() == null) || endTime.before(getEndTime())) {
            return endTime;
        }
        return getEndTime();
    }
    
    protected void restartSimpleTrigger(Date startTime, Calendar calendar) {
        simpleDelegate.setNextFireTime(null);
        simpleDelegate.setPreviousFireTime(null);
        simpleDelegate.setTimesTriggered(0);
        simpleDelegate.setEndTime(null);
        
        Date endTime = calculateEndTime(true, startTime);
        simpleDelegate.setStartTime(startTime);
        simpleDelegate.setEndTime(endTime);
        simpleDelegate.computeFirstFireTime(calendar);
        
        if (I18n.finerLoggable(log())) {
            Date dispEndTime = calculateEndTime(false, startTime);
            I18n.finer(log(), "SCHEDBC-2003: {0} Reset SimpleTrigger "  //NOI18N
                    + "delegate that starts {1}, ends {2} (inclusive), "//NOI18N
                    + "repeats {3} times every {4} millisecs",          //NOI18N
                    whoWhen(),
                    timeStr(simpleDelegate.getStartTime()),
                    timeStr(dispEndTime),
                    repeatStr(simpleDelegate.getRepeatCount()),
                    Long.toString(simpleDelegate.getRepeatInterval()));
        }
    }
    
    protected SimpleTrigger createSimpleTrigger(Date startTime,
            Calendar calendar) {
        Date endTime = calculateEndTime(true, startTime);
        SimpleTrigger sTrig = new SimpleTrigger(getName() + SIMPLE_TRIG_SUFFIX,
                getGroup() + SIMPLE_TRIG_SUFFIX, startTime, endTime,
                getRepeatCount(), getRepeatInterval());
        sTrig.computeFirstFireTime(calendar);
        return sTrig;
    }

    @Override
    public Date computeFirstFireTime(Calendar calendar) {
        quartzCal = calendar;
        
        Date firstTime = cronDelegate.computeFirstFireTime(calendar);
        if (firstTime != null) {
            cronLastTriggered = firstTime.getTime();
        }
        
        whoToTriggerNext();
        
        return firstTime;
    }

    @Override
    public int executionComplete(JobExecutionContext context,
            JobExecutionException result) {
        if (I18n.finestLoggable(log())) {
            I18n.finest(log(), "SCHEDBC-1026: {0} Calling "             //NOI18N
                    + "HybridTrigger#executionComplete()",              //NOI18N
                    whoWhen());
        }
        
        if ((result != null) && result.refireImmediately()) {
            return INSTRUCTION_RE_EXECUTE_JOB;
        }

        if ((result != null) && result.unscheduleFiringTrigger()) {
            return INSTRUCTION_SET_TRIGGER_COMPLETE;
        }

        if ((result != null) && result.unscheduleAllTriggers()) {
            return INSTRUCTION_SET_ALL_JOB_TRIGGERS_COMPLETE;
        }

        if (!mayFireAgain()) {
            return INSTRUCTION_DELETE_TRIGGER;
        }

        return INSTRUCTION_NOOP;
    }

    @Override
    public boolean mayFireAgain() {
        return simpleDelegate.mayFireAgain() || cronDelegate.mayFireAgain();
    }

    @Override
    public Date getStartTime() {
        return cronDelegate.getStartTime();
    }

    @Override
    public void setStartTime(Date startTime) {
        cronDelegate.setStartTime(startTime);
    }

    @Override
    public void setEndTime(Date endTime) {
        cronDelegate.setEndTime(endTime);
    }

    @Override
    public Date getEndTime() {
        return cronDelegate.getEndTime();
    }
    
    public TimeZone getTimeZone() {
        return cronDelegate.getTimeZone();
    }
    
    public void setTimeZone(TimeZone timeZone) {
        cronDelegate.setTimeZone(timeZone);
    }

    @Override
    public Date getNextFireTime() {
        if (cronToBeTriggered) {
            if (I18n.finestLoggable(log())) {
                I18n.finest(log(), "SCHEDBC-1018: {0} Calling "         //NOI18N
                        + "CronTrigger delegate getNextFireTime()",     //NOI18N
                        whoWhen());
            }
            
            return cronDelegate.getNextFireTime();
        } else if (simpleToBeTriggered) {
            if (I18n.finestLoggable(log())) {
                I18n.finest(log(), "SCHEDBC-1019: {0} Calling "         //NOI18N
                        + "SimpleTrigger delegate getNextFireTime()",   //NOI18N
                        whoWhen());
            }

            return simpleDelegate.getNextFireTime();
        }
        return null;
    }

    @Override
    public Date getPreviousFireTime() {
        if (cronJustTriggered) {
            return cronDelegate.getPreviousFireTime();
        } else if (simpleJustTriggered) {
            return simpleDelegate.getPreviousFireTime();
        }
        return null;
    }

    @Override
    public Date getFireTimeAfter(Date afterTime) {
        Date cronAfter = cronDelegate.getFireTimeAfter(afterTime);
        Date simpleAfter = simpleDelegate.getFireTimeAfter(afterTime);
        if (equals(cronAfter, simpleAfter) || before(cronAfter, simpleAfter)) {
            if (I18n.finestLoggable(log())) {
                I18n.finest(log(), "SCHEDBC-1022: {0} Called "          //NOI18N
                        + "CronTrigger delegate getFireTimeAfter()",    //NOI18N
                        whoWhen());
            }
            return cronAfter;
        } else {
            if (I18n.finestLoggable(log())) {
                I18n.finest(log(), "SCHEDBC-1023: {0} Called "          //NOI18N
                        + "SimpleTrigger delegate getFireTimeAfter()",  //NOI18N
                        whoWhen());
            }
            return simpleAfter;
        }
    }

    @Override
    public Date getFinalFireTime() {
        Date finalTime = cronDelegate.getFinalFireTime();
        if ((finalTime != null) && (quartzCal != null)) {
            if (getRepeatCount() != SimpleTrigger.REPEAT_INDEFINITELY) {
                SimpleTrigger sTrig = createSimpleTrigger(finalTime, quartzCal);
                finalTime = sTrig.getFinalFireTime();
                
                if (I18n.finestLoggable(log())) {
                    I18n.finest(log(), "SCHEDBC-1024: {0} Called "      //NOI18N
                            + "SimpleTrigger delegate "                 //NOI18N
                            + "getFinalFireTime()",                     //NOI18N
                            whoWhen());
                }
            }
        } else {
            if (I18n.finestLoggable(log())) {
                I18n.finest(log(), "SCHEDBC-1025: {0} Called "          //NOI18N
                        + "CronTrigger delegate getFinalFireTime()",    //NOI18N
                        whoWhen());
            }
        }
        return finalTime;
    }

    @Override
    public void setMisfireInstruction(int misfireInstruction) {
        super.setMisfireInstruction(misfireInstruction);
        switch (misfireInstruction) {
        case MISFIRE_INSTRUCTION_RESCHEDULE_NOW_WITH_EXISTING_REPEAT_COUNT:
        case MISFIRE_INSTRUCTION_RESCHEDULE_NOW_WITH_REMAINING_COUNT:
            cronDelegate.setMisfireInstruction(
                    CronTrigger.MISFIRE_INSTRUCTION_FIRE_ONCE_NOW);
            simpleDelegate.setMisfireInstruction(misfireInstruction);
            break;
        case MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_EXISTING_COUNT:
        case MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_REMAINING_COUNT:
            cronDelegate.setMisfireInstruction(
                    CronTrigger.MISFIRE_INSTRUCTION_FIRE_ONCE_NOW);
            simpleDelegate.setMisfireInstruction(misfireInstruction);
            break;
        case MISFIRE_INSTRUCTION_SMART_POLICY:
        default:
            cronDelegate.setMisfireInstruction(
                    CronTrigger.MISFIRE_INSTRUCTION_FIRE_ONCE_NOW);
            simpleDelegate.setMisfireInstruction(SimpleTrigger
                    .MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_EXISTING_COUNT);
            break;
        }
    }

    @Override
    protected boolean validateMisfireInstruction(int misfireInstruction) {
        switch (misfireInstruction) {
        case MISFIRE_INSTRUCTION_SMART_POLICY:
        case MISFIRE_INSTRUCTION_RESCHEDULE_NOW_WITH_EXISTING_REPEAT_COUNT:
        case MISFIRE_INSTRUCTION_RESCHEDULE_NOW_WITH_REMAINING_COUNT:
        case MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_EXISTING_COUNT:
        case MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_REMAINING_COUNT:
            return true;
        }
        return false;
    }

    @Override
    public void updateAfterMisfire(Calendar cal) {
        if (simpleToBeTriggered) {
            simpleDelegate.updateAfterMisfire(cal);
            if (simpleDelegate.getNextFireTime() == null) {
                cronToBeTriggered = true;
            }
        }
        if (cronToBeTriggered) {
            cronDelegate.updateAfterMisfire(cal);
            
            // Verify reschedule next time falls on right boundary
            if ((simpleDelegate.getNextFireTime() == null)
                    || (SimpleTrigger
                    .MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_EXISTING_COUNT
                        == simpleDelegate.getMisfireInstruction())
                    || (SimpleTrigger
                    .MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_REMAINING_COUNT
                        == simpleDelegate.getMisfireInstruction())) {
                Date cronNext = cronDelegate.getNextFireTime();
                if ((cronNext != null)
                        && !getCronExpressionHelper().isSatisfiedBy(cronNext)) {
                    Date cronGood = computeNextCronTimeAfterMisfire(cronNext);
                    if (cronGood != null) {
                        if (cronGood.before(cronNext)) {
                            // We're starting in middle of begin --> end duration
                            // period, so simulate a trigger for the cron trigger
                            // time just prior to this
                            I18n.finest(log(), "SCHEDBC-1033: "         //NOI18N
                                    + "Simulating CronTrigger#"         //NOI18N
                                    + "triggered() call for prior "     //NOI18N
                                    + "missed period");                 //NOI18N
                            cronDelegate.setNextFireTime(cronGood);
                            cronDelegate.triggered(cal);
                            cronLastTriggered = cronGood.getTime();

                            restartSimpleTrigger(cronGood, cal);
                            simpleDelegate.updateAfterMisfire(cal);
                        } else {
                            cronDelegate.setPreviousFireTime(
                                    new Date(cronLastTriggered));
                            cronDelegate.setNextFireTime(cronGood);
                        }
                    } else {
                        cronDelegate.setNextFireTime(null);
                    }
                }
            }
        }
        
        whoToTriggerNext();
        
        if (getNextFireTime() != null) {
            I18n.fine(log(), "SCHEDBC-3006: Trigger will resume "       //NOI18N
                    + "after misfire at {0}",                           //NOI18N
                    XSD_STD_SDF.format(getNextFireTime()));
        }
    }
    
    private Date computeNextCronTimeAfterMisfire(Date nonCronTime) {
        if (nonCronTime != null) {
            Date lastTime = new Date(cronLastTriggered);
            Date guess = getCronExpressionHelper()
                    .getNextValidTimeAfter(lastTime);
            Date before = guess;
            if (I18n.finestLoggable(log())) {
                I18n.finest(log(), "SCHEDBC-1034: Last Cron time={0}, " //NOI18N
                        + "finding next available Cron time nearest "   //NOI18N
                        + "{1}: {2}", timeStr(lastTime),                //NOI18N
                        timeStr(nonCronTime), timeStr(guess));
            }
            while ((guess != null) && guess.before(nonCronTime)) {
                before = guess;
                guess = getCronExpressionHelper().getNextValidTimeAfter(
                    guess);
                if (I18n.finestLoggable(log())) {
                    I18n.finest(log(), "SCHEDBC-1035: Finding next "    //NOI18N
                            + "available Cron time nearest {0}: {1}",   //NOI18N
                            timeStr(nonCronTime), timeStr(guess));
                }
            }
            return before;
        }
        return null;
    }

    @Override
    public void updateWithNewCalendar(Calendar cal, long misfireThreshold) {
        simpleDelegate.updateWithNewCalendar(cal, misfireThreshold);
        cronDelegate.updateWithNewCalendar(cal, misfireThreshold);
        
        whoToTriggerNext();
    }
    
    private String whoCalledMe() {
        if (Boolean.getBoolean("org.glassfish.openesb.schedulerbc"      //NOI18N
                + ".domain.HybridTrigger.whoCalledMe")) {
            Throwable t = new Throwable();
            StackTraceElement[] stack = t.getStackTrace();
            StringBuilder sb = new StringBuilder();
            for (int i = Math.min(6, stack.length) - 1; i > 1; i--) {
                sb.append(NULINE).append('\t');                         //NOI18N
                sb.append(stack[i].getClassName()).append('#')          //NOI18N
                    .append(stack[i].getMethodName()).append(":L")      //NOI18N
                    .append(stack[i].getLineNumber()).append(" ==vv");  //NOI18N
            }
            sb.append(NULINE).append('\t').append('\t');                //NOI18N
            return sb.toString();
        }
        return "";                                                      //NOI18N
    }

    private class SimpleTriggerProxy extends SimpleTrigger {
        public SimpleTriggerProxy(String name, String group) {
            super(name, group);
        }

        @Override
        public void setNextFireTime(Date nextFireTime) {
            I18n.finest(log(), "SCHEDBC-1029: {0} {1}"                  //NOI18N
                    + "SimpleTrigger#setNextFireTime({2}) called",      //NOI18N
                    whoWhen(), whoCalledMe(), timeStr(nextFireTime));
            super.setNextFireTime(nextFireTime);
        }

        @Override
        public void setPreviousFireTime(Date prevFireTime) {
            I18n.finest(log(), "SCHEDBC-1030: {0} {1}"                  //NOI18N
                    + "SimpleTrigger#setPreviousFireTime({2}) called",  //NOI18N
                    whoWhen(), whoCalledMe(), timeStr(prevFireTime));
            super.setPreviousFireTime(prevFireTime);
        }

        @Override
        protected boolean validateMisfireInstruction(int misfireInstruction) {
            return super.validateMisfireInstruction(misfireInstruction);
        }
    }
    
    private class CronTriggerProxy extends CronTrigger {
        public CronTriggerProxy(String name, String group, String cronExpr)
                throws ParseException {
            super(name, group, cronExpr);
        }

        @Override
        public void setNextFireTime(Date nextFireTime) {
            I18n.finest(log(), "SCHEDBC-1031: {0} {1}"                  //NOI18N
                    + "CronTrigger#setNextFireTime({2}) called",        //NOI18N
                    whoWhen(), whoCalledMe(), timeStr(nextFireTime));
            super.setNextFireTime(nextFireTime);
        }

        @Override
        public void setPreviousFireTime(Date prevFireTime) {
            I18n.finest(log(), "SCHEDBC-1032: {0} {1}"                  //NOI18N
                    + "CronTrigger#setPreviousFireTime({2}) called",    //NOI18N
                    whoWhen(), whoCalledMe(), timeStr(prevFireTime));
            super.setPreviousFireTime(prevFireTime);
        }

        @Override
        protected boolean validateMisfireInstruction(int misfireInstruction) {
            return super.validateMisfireInstruction(misfireInstruction);
        }
    }
}
