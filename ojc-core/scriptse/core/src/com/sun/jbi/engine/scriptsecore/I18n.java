/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author rchen
 */
package com.sun.jbi.engine.scriptsecore;

import java.util.regex.Pattern;

import com.sun.jbi.common.util.LocalizationSupport;

public class I18n extends LocalizationSupport {
	private static final I18n mI18n = new I18n();
	
	protected I18n() {
		super(Pattern.compile("(SCPTSECORE-[1-4]\\d\\d\\d)(: )(.*)", Pattern.DOTALL), 
			  "", null);
	}
	
	public static String loc(String message, Object... params) {
		return mI18n.t(message, params);
	}
}
