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
 * @(#)$Id: CommandContext.java,v 1.1 2007/09/27 16:51:00 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool;

import java.util.HashMap;
import java.util.Map;

import com.sun.jbi.engine.bpel.monitor.tool.util.CommandUtil;
import com.sun.jbi.engine.bpel.monitor.tool.util.CommandUtil.CommandData;

/**
 * The interface that receives the user command, navigates to
 * the parent or child context, provides help for the command
 * or executes the command
 * 
 * @author Sun Microsystems
 *
 */
public abstract class CommandContext {
	
	public static String COMMAND_PROMPT = ">";
	
	protected String mName;
	protected CommandContext mParent;
	protected ContextResource mResource;
	
	
	protected Map<String, CommandContext> mChildContexts = new HashMap<String, CommandContext> ();
	protected Map<String, Command> mChildCommands = new HashMap<String, Command> ();
	
	public CommandContext (String name, CommandContext parent, ContextResource resource) {
		mName = name;
		mParent = parent;
		mResource = resource;
		mChildCommands.put("help", new HelpCommand ("Help", this));
		mChildCommands.put("h", new HelpCommand ("Help", this));
		mChildCommands.put("e", new ExitCommand ("e", this));
		mChildCommands.put("exit", new ExitCommand ("e", this));
	}
	
	public CommandContext exit () {
		if (mParent != null) {
			mParent.display();
			return mParent;
		} else {
			System.exit(0);
		}
		return null;
	}
	
	public CommandContext getChildContext (CommandData command) {
		return lookupChildContext (command);
	}
	
	
	public String getName () {
		return mName;
	}
	
	public CommandContext getParent () {
		return mParent;
	}

	/**
	 * Look up the concrete command based on the current context, returns null
	 * if nothing is found
	 * @param cmdData
	 * @return The command by the name in the current context or null
	 */
	protected  Command lookupCommand(CommandData cmdData) {
		// TODO Auto-generated method stub
		String name = cmdData.name;
		return mChildCommands.get(name);
	}
	
	/**
	 * Look up a child context based on the current context, returns null
	 * if nothing is found
	 * @param cmdData
	 * @return The command by the name in the current context or null
	 */
	protected  CommandContext lookupChildContext(CommandData command) {
		// TODO Auto-generated method stub
		String name = command.name;
		return mChildContexts.get(name);
	}

	/**
	 * Show all the commands available for the current context.
	 *
	 */
	public  void  display () {
		String displayStr = CommandUtil.getContextDisplay(mName);
		System.out.println(displayStr);
//		CommandUtil.displayPrompt();
	}

	public ContextResource getResource () {
		return mResource;
	}
}
