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
 * @(#)Command.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.commands;

public abstract class Command  {

private String name;


/**
 * Create a Command with no name.
 */
public Command () { }


/**
 * Create a Command with the specified name.
 * @param name of the Command.
 */
public Command (String name) {
	setName(name);
}


/**
 * @return <code>true</code> if the command can be executed
 */
public boolean canExecute() {
	return true;
}


/**
 * @return <code>true</code> if the command can be undone. 
 * This method should only be
 * called after <code>execute()</code> or <code>redo()</code> has been called.
 */
public abstract boolean canUndo();
	

/**
 * This is called to indicate that the <code>Command</code> will not be used again. The
 * Command may be in any state (executed, undone or redone) when dispose is called. The
 * Command should not be referenced in any way after it has been disposed.
 */
public void dispose() { 
}


/**
 * executes the Command. This method should not be called if the Command is not
 * executable.
 */
public abstract void execute();



/**
 * @return a String used to describe this command to the User
 */
public String getName() {
	return name;
}

/**
 * Re-executes the Command. This method should only be called after <code>undo()</code>
 * has been called.
 */
public void redo() {
	execute();
}


/**
 * Sets the name used to describe this command to the User.
 * @param name the command
 */
public void setName(String name) {
	this.name = name;
}

/**
 * Undoes the changes performed during <code>execute()</code>. This method should only be
 * called after <code>execute</code> has been called, and only when <code>canUndo()</code>
 * returns <code>true</code>.
 * @see #canUndo()
 */
public abstract void undo();

}
