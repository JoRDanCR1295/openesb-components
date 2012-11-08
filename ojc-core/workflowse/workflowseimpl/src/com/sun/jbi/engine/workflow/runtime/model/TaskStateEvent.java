package com.sun.jbi.engine.workflow.runtime.model;

import java.util.EventObject;

import com.sun.jbi.workflow.model.Escalation;

public class TaskStateEvent extends EventObject {

    private RuntimeTask.TaskState mOldState;

    private RuntimeTask.TaskState mNewState;

    private RuntimeTask mTask;

    private Object mModelRef;

    public TaskStateEvent(Object source, RuntimeTask.TaskState oldState,
            RuntimeTask.TaskState newState, RuntimeTask task) {
        super(source);
        this.mOldState = oldState;
        this.mNewState = newState;
        this.mTask = task;
    }

    public TaskStateEvent(Object source, RuntimeTask.TaskState oldState,
            RuntimeTask.TaskState newState, RuntimeTask task, Object modelRef) {
        super(source);
        this.mOldState = oldState;
        this.mNewState = newState;
        this.mTask = task;
        this.mModelRef = modelRef;
    }

    public RuntimeTask.TaskState getOldState() {
        return this.mOldState;
    }

    public RuntimeTask.TaskState getNewState() {
        return this.mNewState;
    }

    public RuntimeTask getTask() {
        return this.mTask;
    }
    
    public Object getModelRef () {
        return this.mModelRef;
    }

}
