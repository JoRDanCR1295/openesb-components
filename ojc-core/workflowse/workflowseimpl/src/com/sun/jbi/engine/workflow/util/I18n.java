package com.sun.jbi.engine.workflow.util;

import java.util.regex.Pattern;

import com.sun.jbi.common.util.LocalizationSupport;

/**
 * Internationalization utility for BPEL Core.
 * @author Sun Microsystems
 */
public class I18n extends LocalizationSupport {
	private static final I18n mI18n = new I18n();
	
	protected I18n() {
		super(Pattern.compile("(WLM-[4-7]\\d\\d\\d)(: )(.*)", Pattern.DOTALL), 
			  "", null);
	}
	
	public static String loc(String message, Object... params) {
		return mI18n.t(message, params);
	}
}