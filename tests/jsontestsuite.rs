#![allow(non_snake_case)]
use justjson::parser::Parser;

macro_rules! json_test_suite_test {
    ($name:ident, $file_name:literal) => {
        #[test]
        fn $name() {
            let src = include_bytes!(concat!("./JSONTestSuite/", $file_name));

            let result = Parser::validate_json_bytes(src);
            match stringify!($name).chars().next().unwrap() {
                'i' => {
                    // Doesn't matter, just don't crash.
                    println!("i test result: {}", result.is_ok())
                }
                'y' => {
                    result.expect("failed when expecting success");
                }
                other => {
                    assert_eq!(other, 'n');
                    result.expect_err("success when expecting failure");
                }
            }
        }
    };
}

#[test]
fn generate_macro_calls() {
    let mut names = Vec::new();
    for entry in std::fs::read_dir("./tests/JSONTestSuite").unwrap() {
        let name = entry.unwrap().file_name();
        let name = name.to_str().unwrap();
        if name.ends_with(".json") {
            names.push(name.to_string());
        }
    }
    names.sort();
    for name in names {
        let name_without_json = &name[..name.len() - 5];
        let safe = name_without_json
            .replace('+', "plus")
            .replace(['-', '.'], "_")
            .replace('#', "pound");
        println!("json_test_suite_test!({safe}, \"{name}\");");
    }
}

json_test_suite_test!(
    i_number_double_huge_neg_exp,
    "i_number_double_huge_neg_exp.json"
);
json_test_suite_test!(i_number_huge_exp, "i_number_huge_exp.json");
json_test_suite_test!(i_number_neg_int_huge_exp, "i_number_neg_int_huge_exp.json");
json_test_suite_test!(
    i_number_pos_double_huge_exp,
    "i_number_pos_double_huge_exp.json"
);
json_test_suite_test!(
    i_number_real_neg_overflow,
    "i_number_real_neg_overflow.json"
);
json_test_suite_test!(
    i_number_real_pos_overflow,
    "i_number_real_pos_overflow.json"
);
json_test_suite_test!(i_number_real_underflow, "i_number_real_underflow.json");
json_test_suite_test!(i_number_too_big_neg_int, "i_number_too_big_neg_int.json");
json_test_suite_test!(i_number_too_big_pos_int, "i_number_too_big_pos_int.json");
json_test_suite_test!(
    i_number_very_big_negative_int,
    "i_number_very_big_negative_int.json"
);
json_test_suite_test!(
    i_object_key_lone_2nd_surrogate,
    "i_object_key_lone_2nd_surrogate.json"
);
json_test_suite_test!(
    i_string_1st_surrogate_but_2nd_missing,
    "i_string_1st_surrogate_but_2nd_missing.json"
);
json_test_suite_test!(
    i_string_1st_valid_surrogate_2nd_invalid,
    "i_string_1st_valid_surrogate_2nd_invalid.json"
);
json_test_suite_test!(
    i_string_UTF_16LE_with_BOM,
    "i_string_UTF-16LE_with_BOM.json"
);
json_test_suite_test!(
    i_string_UTF_8_invalid_sequence,
    "i_string_UTF-8_invalid_sequence.json"
);
json_test_suite_test!(
    i_string_UTF8_surrogate_UplusD800,
    "i_string_UTF8_surrogate_U+D800.json"
);
json_test_suite_test!(
    i_string_incomplete_surrogate_and_escape_valid,
    "i_string_incomplete_surrogate_and_escape_valid.json"
);
json_test_suite_test!(
    i_string_incomplete_surrogate_pair,
    "i_string_incomplete_surrogate_pair.json"
);
json_test_suite_test!(
    i_string_incomplete_surrogates_escape_valid,
    "i_string_incomplete_surrogates_escape_valid.json"
);
json_test_suite_test!(
    i_string_invalid_lonely_surrogate,
    "i_string_invalid_lonely_surrogate.json"
);
json_test_suite_test!(
    i_string_invalid_surrogate,
    "i_string_invalid_surrogate.json"
);
json_test_suite_test!(i_string_invalid_utf_8, "i_string_invalid_utf-8.json");
json_test_suite_test!(
    i_string_inverted_surrogates_Uplus1D11E,
    "i_string_inverted_surrogates_U+1D11E.json"
);
json_test_suite_test!(i_string_iso_latin_1, "i_string_iso_latin_1.json");
json_test_suite_test!(
    i_string_lone_second_surrogate,
    "i_string_lone_second_surrogate.json"
);
json_test_suite_test!(
    i_string_lone_utf8_continuation_byte,
    "i_string_lone_utf8_continuation_byte.json"
);
json_test_suite_test!(
    i_string_not_in_unicode_range,
    "i_string_not_in_unicode_range.json"
);
json_test_suite_test!(
    i_string_overlong_sequence_2_bytes,
    "i_string_overlong_sequence_2_bytes.json"
);
json_test_suite_test!(
    i_string_overlong_sequence_6_bytes,
    "i_string_overlong_sequence_6_bytes.json"
);
json_test_suite_test!(
    i_string_overlong_sequence_6_bytes_null,
    "i_string_overlong_sequence_6_bytes_null.json"
);
json_test_suite_test!(i_string_truncated_utf_8, "i_string_truncated-utf-8.json");
json_test_suite_test!(i_string_utf16BE_no_BOM, "i_string_utf16BE_no_BOM.json");
json_test_suite_test!(i_string_utf16LE_no_BOM, "i_string_utf16LE_no_BOM.json");
json_test_suite_test!(
    i_structure_500_nested_arrays,
    "i_structure_500_nested_arrays.json"
);
json_test_suite_test!(
    i_structure_UTF_8_BOM_empty_object,
    "i_structure_UTF-8_BOM_empty_object.json"
);
json_test_suite_test!(
    n_array_1_true_without_comma,
    "n_array_1_true_without_comma.json"
);
json_test_suite_test!(n_array_a_invalid_utf8, "n_array_a_invalid_utf8.json");
json_test_suite_test!(
    n_array_colon_instead_of_comma,
    "n_array_colon_instead_of_comma.json"
);
json_test_suite_test!(n_array_comma_after_close, "n_array_comma_after_close.json");
json_test_suite_test!(n_array_comma_and_number, "n_array_comma_and_number.json");
json_test_suite_test!(n_array_double_comma, "n_array_double_comma.json");
json_test_suite_test!(
    n_array_double_extra_comma,
    "n_array_double_extra_comma.json"
);
json_test_suite_test!(n_array_extra_close, "n_array_extra_close.json");
json_test_suite_test!(n_array_extra_comma, "n_array_extra_comma.json");
json_test_suite_test!(n_array_incomplete, "n_array_incomplete.json");
json_test_suite_test!(
    n_array_incomplete_invalid_value,
    "n_array_incomplete_invalid_value.json"
);
json_test_suite_test!(
    n_array_inner_array_no_comma,
    "n_array_inner_array_no_comma.json"
);
json_test_suite_test!(n_array_invalid_utf8, "n_array_invalid_utf8.json");
json_test_suite_test!(
    n_array_items_separated_by_semicolon,
    "n_array_items_separated_by_semicolon.json"
);
json_test_suite_test!(n_array_just_comma, "n_array_just_comma.json");
json_test_suite_test!(n_array_just_minus, "n_array_just_minus.json");
json_test_suite_test!(n_array_missing_value, "n_array_missing_value.json");
json_test_suite_test!(n_array_newlines_unclosed, "n_array_newlines_unclosed.json");
json_test_suite_test!(n_array_number_and_comma, "n_array_number_and_comma.json");
json_test_suite_test!(
    n_array_number_and_several_commas,
    "n_array_number_and_several_commas.json"
);
json_test_suite_test!(
    n_array_spaces_vertical_tab_formfeed,
    "n_array_spaces_vertical_tab_formfeed.json"
);
json_test_suite_test!(n_array_star_inside, "n_array_star_inside.json");
json_test_suite_test!(n_array_unclosed, "n_array_unclosed.json");
json_test_suite_test!(
    n_array_unclosed_trailing_comma,
    "n_array_unclosed_trailing_comma.json"
);
json_test_suite_test!(
    n_array_unclosed_with_new_lines,
    "n_array_unclosed_with_new_lines.json"
);
json_test_suite_test!(
    n_array_unclosed_with_object_inside,
    "n_array_unclosed_with_object_inside.json"
);
json_test_suite_test!(n_incomplete_false, "n_incomplete_false.json");
json_test_suite_test!(n_incomplete_null, "n_incomplete_null.json");
json_test_suite_test!(n_incomplete_true, "n_incomplete_true.json");
json_test_suite_test!(
    n_multidigit_number_then_00,
    "n_multidigit_number_then_00.json"
);
json_test_suite_test!(n_number_plusplus, "n_number_++.json");
json_test_suite_test!(n_number_plus1, "n_number_+1.json");
json_test_suite_test!(n_number_plusInf, "n_number_+Inf.json");
json_test_suite_test!(n_number__01, "n_number_-01.json");
json_test_suite_test!(n_number__1_0_, "n_number_-1.0..json");
json_test_suite_test!(n_number__2_, "n_number_-2..json");
json_test_suite_test!(n_number__NaN, "n_number_-NaN.json");
json_test_suite_test!(n_number___1, "n_number_.-1.json");
json_test_suite_test!(n_number__2e_3, "n_number_.2e-3.json");
json_test_suite_test!(n_number_0_1_2, "n_number_0.1.2.json");
json_test_suite_test!(n_number_0_3eplus, "n_number_0.3e+.json");
json_test_suite_test!(n_number_0_3e, "n_number_0.3e.json");
json_test_suite_test!(n_number_0_e1, "n_number_0.e1.json");
json_test_suite_test!(n_number_0_capital_Eplus, "n_number_0_capital_E+.json");
json_test_suite_test!(n_number_0_capital_E, "n_number_0_capital_E.json");
json_test_suite_test!(n_number_0eplus, "n_number_0e+.json");
json_test_suite_test!(n_number_0e, "n_number_0e.json");
json_test_suite_test!(n_number_1_0eplus, "n_number_1.0e+.json");
json_test_suite_test!(n_number_1_0e_, "n_number_1.0e-.json");
json_test_suite_test!(n_number_1_0e, "n_number_1.0e.json");
json_test_suite_test!(n_number_1_000, "n_number_1_000.json");
json_test_suite_test!(n_number_1eE2, "n_number_1eE2.json");
json_test_suite_test!(n_number_2_eplus3, "n_number_2.e+3.json");
json_test_suite_test!(n_number_2_e_3, "n_number_2.e-3.json");
json_test_suite_test!(n_number_2_e3, "n_number_2.e3.json");
json_test_suite_test!(n_number_9_eplus, "n_number_9.e+.json");
json_test_suite_test!(n_number_Inf, "n_number_Inf.json");
json_test_suite_test!(n_number_NaN, "n_number_NaN.json");
json_test_suite_test!(
    n_number_UplusFF11_fullwidth_digit_one,
    "n_number_U+FF11_fullwidth_digit_one.json"
);
json_test_suite_test!(n_number_expression, "n_number_expression.json");
json_test_suite_test!(n_number_hex_1_digit, "n_number_hex_1_digit.json");
json_test_suite_test!(n_number_hex_2_digits, "n_number_hex_2_digits.json");
json_test_suite_test!(n_number_infinity, "n_number_infinity.json");
json_test_suite_test!(n_number_invalidplus_, "n_number_invalid+-.json");
json_test_suite_test!(
    n_number_invalid_negative_real,
    "n_number_invalid-negative-real.json"
);
json_test_suite_test!(
    n_number_invalid_utf_8_in_bigger_int,
    "n_number_invalid-utf-8-in-bigger-int.json"
);
json_test_suite_test!(
    n_number_invalid_utf_8_in_exponent,
    "n_number_invalid-utf-8-in-exponent.json"
);
json_test_suite_test!(
    n_number_invalid_utf_8_in_int,
    "n_number_invalid-utf-8-in-int.json"
);
json_test_suite_test!(n_number_minus_infinity, "n_number_minus_infinity.json");
json_test_suite_test!(
    n_number_minus_sign_with_trailing_garbage,
    "n_number_minus_sign_with_trailing_garbage.json"
);
json_test_suite_test!(n_number_minus_space_1, "n_number_minus_space_1.json");
json_test_suite_test!(
    n_number_neg_int_starting_with_zero,
    "n_number_neg_int_starting_with_zero.json"
);
json_test_suite_test!(
    n_number_neg_real_without_int_part,
    "n_number_neg_real_without_int_part.json"
);
json_test_suite_test!(
    n_number_neg_with_garbage_at_end,
    "n_number_neg_with_garbage_at_end.json"
);
json_test_suite_test!(
    n_number_real_garbage_after_e,
    "n_number_real_garbage_after_e.json"
);
json_test_suite_test!(
    n_number_real_with_invalid_utf8_after_e,
    "n_number_real_with_invalid_utf8_after_e.json"
);
json_test_suite_test!(
    n_number_real_without_fractional_part,
    "n_number_real_without_fractional_part.json"
);
json_test_suite_test!(
    n_number_starting_with_dot,
    "n_number_starting_with_dot.json"
);
json_test_suite_test!(n_number_with_alpha, "n_number_with_alpha.json");
json_test_suite_test!(n_number_with_alpha_char, "n_number_with_alpha_char.json");
json_test_suite_test!(
    n_number_with_leading_zero,
    "n_number_with_leading_zero.json"
);
json_test_suite_test!(n_object_bad_value, "n_object_bad_value.json");
json_test_suite_test!(n_object_bracket_key, "n_object_bracket_key.json");
json_test_suite_test!(
    n_object_comma_instead_of_colon,
    "n_object_comma_instead_of_colon.json"
);
json_test_suite_test!(n_object_double_colon, "n_object_double_colon.json");
json_test_suite_test!(n_object_emoji, "n_object_emoji.json");
json_test_suite_test!(n_object_garbage_at_end, "n_object_garbage_at_end.json");
json_test_suite_test!(
    n_object_key_with_single_quotes,
    "n_object_key_with_single_quotes.json"
);
json_test_suite_test!(
    n_object_lone_continuation_byte_in_key_and_trailing_comma,
    "n_object_lone_continuation_byte_in_key_and_trailing_comma.json"
);
json_test_suite_test!(n_object_missing_colon, "n_object_missing_colon.json");
json_test_suite_test!(n_object_missing_key, "n_object_missing_key.json");
json_test_suite_test!(
    n_object_missing_semicolon,
    "n_object_missing_semicolon.json"
);
json_test_suite_test!(n_object_missing_value, "n_object_missing_value.json");
json_test_suite_test!(n_object_no_colon, "n_object_no-colon.json");
json_test_suite_test!(n_object_non_string_key, "n_object_non_string_key.json");
json_test_suite_test!(
    n_object_non_string_key_but_huge_number_instead,
    "n_object_non_string_key_but_huge_number_instead.json"
);
json_test_suite_test!(
    n_object_repeated_null_null,
    "n_object_repeated_null_null.json"
);
json_test_suite_test!(
    n_object_several_trailing_commas,
    "n_object_several_trailing_commas.json"
);
json_test_suite_test!(n_object_single_quote, "n_object_single_quote.json");
json_test_suite_test!(n_object_trailing_comma, "n_object_trailing_comma.json");
json_test_suite_test!(n_object_trailing_comment, "n_object_trailing_comment.json");
json_test_suite_test!(
    n_object_trailing_comment_open,
    "n_object_trailing_comment_open.json"
);
json_test_suite_test!(
    n_object_trailing_comment_slash_open,
    "n_object_trailing_comment_slash_open.json"
);
json_test_suite_test!(
    n_object_trailing_comment_slash_open_incomplete,
    "n_object_trailing_comment_slash_open_incomplete.json"
);
json_test_suite_test!(
    n_object_two_commas_in_a_row,
    "n_object_two_commas_in_a_row.json"
);
json_test_suite_test!(n_object_unquoted_key, "n_object_unquoted_key.json");
json_test_suite_test!(
    n_object_unterminated_value,
    "n_object_unterminated-value.json"
);
json_test_suite_test!(
    n_object_with_single_string,
    "n_object_with_single_string.json"
);
json_test_suite_test!(
    n_object_with_trailing_garbage,
    "n_object_with_trailing_garbage.json"
);
json_test_suite_test!(n_single_space, "n_single_space.json");
json_test_suite_test!(
    n_string_1_surrogate_then_escape,
    "n_string_1_surrogate_then_escape.json"
);
json_test_suite_test!(
    n_string_1_surrogate_then_escape_u,
    "n_string_1_surrogate_then_escape_u.json"
);
json_test_suite_test!(
    n_string_1_surrogate_then_escape_u1,
    "n_string_1_surrogate_then_escape_u1.json"
);
json_test_suite_test!(
    n_string_1_surrogate_then_escape_u1x,
    "n_string_1_surrogate_then_escape_u1x.json"
);
json_test_suite_test!(
    n_string_accentuated_char_no_quotes,
    "n_string_accentuated_char_no_quotes.json"
);
json_test_suite_test!(n_string_backslash_00, "n_string_backslash_00.json");
json_test_suite_test!(n_string_escape_x, "n_string_escape_x.json");
json_test_suite_test!(
    n_string_escaped_backslash_bad,
    "n_string_escaped_backslash_bad.json"
);
json_test_suite_test!(
    n_string_escaped_ctrl_char_tab,
    "n_string_escaped_ctrl_char_tab.json"
);
json_test_suite_test!(n_string_escaped_emoji, "n_string_escaped_emoji.json");
json_test_suite_test!(
    n_string_incomplete_escape,
    "n_string_incomplete_escape.json"
);
json_test_suite_test!(
    n_string_incomplete_escaped_character,
    "n_string_incomplete_escaped_character.json"
);
json_test_suite_test!(
    n_string_incomplete_surrogate,
    "n_string_incomplete_surrogate.json"
);
json_test_suite_test!(
    n_string_incomplete_surrogate_escape_invalid,
    "n_string_incomplete_surrogate_escape_invalid.json"
);
json_test_suite_test!(
    n_string_invalid_utf_8_in_escape,
    "n_string_invalid-utf-8-in-escape.json"
);
json_test_suite_test!(
    n_string_invalid_backslash_esc,
    "n_string_invalid_backslash_esc.json"
);
json_test_suite_test!(
    n_string_invalid_unicode_escape,
    "n_string_invalid_unicode_escape.json"
);
json_test_suite_test!(
    n_string_invalid_utf8_after_escape,
    "n_string_invalid_utf8_after_escape.json"
);
json_test_suite_test!(
    n_string_leading_uescaped_thinspace,
    "n_string_leading_uescaped_thinspace.json"
);
json_test_suite_test!(
    n_string_no_quotes_with_bad_escape,
    "n_string_no_quotes_with_bad_escape.json"
);
json_test_suite_test!(
    n_string_single_doublequote,
    "n_string_single_doublequote.json"
);
json_test_suite_test!(n_string_single_quote, "n_string_single_quote.json");
json_test_suite_test!(
    n_string_single_string_no_double_quotes,
    "n_string_single_string_no_double_quotes.json"
);
json_test_suite_test!(
    n_string_start_escape_unclosed,
    "n_string_start_escape_unclosed.json"
);
json_test_suite_test!(
    n_string_unescaped_ctrl_char,
    "n_string_unescaped_ctrl_char.json"
);
json_test_suite_test!(
    n_string_unescaped_newline,
    "n_string_unescaped_newline.json"
);
json_test_suite_test!(n_string_unescaped_tab, "n_string_unescaped_tab.json");
json_test_suite_test!(n_string_unicode_CapitalU, "n_string_unicode_CapitalU.json");
json_test_suite_test!(
    n_string_with_trailing_garbage,
    "n_string_with_trailing_garbage.json"
);
json_test_suite_test!(
    n_structure_100000_opening_arrays,
    "n_structure_100000_opening_arrays.json"
);
json_test_suite_test!(
    n_structure_Uplus2060_word_joined,
    "n_structure_U+2060_word_joined.json"
);
json_test_suite_test!(
    n_structure_UTF8_BOM_no_data,
    "n_structure_UTF8_BOM_no_data.json"
);
json_test_suite_test!(
    n_structure_angle_bracket__,
    "n_structure_angle_bracket_..json"
);
json_test_suite_test!(
    n_structure_angle_bracket_null,
    "n_structure_angle_bracket_null.json"
);
json_test_suite_test!(
    n_structure_array_trailing_garbage,
    "n_structure_array_trailing_garbage.json"
);
json_test_suite_test!(
    n_structure_array_with_extra_array_close,
    "n_structure_array_with_extra_array_close.json"
);
json_test_suite_test!(
    n_structure_array_with_unclosed_string,
    "n_structure_array_with_unclosed_string.json"
);
json_test_suite_test!(
    n_structure_ascii_unicode_identifier,
    "n_structure_ascii-unicode-identifier.json"
);
json_test_suite_test!(
    n_structure_capitalized_True,
    "n_structure_capitalized_True.json"
);
json_test_suite_test!(
    n_structure_close_unopened_array,
    "n_structure_close_unopened_array.json"
);
json_test_suite_test!(
    n_structure_comma_instead_of_closing_brace,
    "n_structure_comma_instead_of_closing_brace.json"
);
json_test_suite_test!(n_structure_double_array, "n_structure_double_array.json");
json_test_suite_test!(n_structure_end_array, "n_structure_end_array.json");
json_test_suite_test!(
    n_structure_incomplete_UTF8_BOM,
    "n_structure_incomplete_UTF8_BOM.json"
);
json_test_suite_test!(
    n_structure_lone_invalid_utf_8,
    "n_structure_lone-invalid-utf-8.json"
);
json_test_suite_test!(
    n_structure_lone_open_bracket,
    "n_structure_lone-open-bracket.json"
);
json_test_suite_test!(n_structure_no_data, "n_structure_no_data.json");
json_test_suite_test!(
    n_structure_null_byte_outside_string,
    "n_structure_null-byte-outside-string.json"
);
json_test_suite_test!(
    n_structure_number_with_trailing_garbage,
    "n_structure_number_with_trailing_garbage.json"
);
json_test_suite_test!(
    n_structure_object_followed_by_closing_object,
    "n_structure_object_followed_by_closing_object.json"
);
json_test_suite_test!(
    n_structure_object_unclosed_no_value,
    "n_structure_object_unclosed_no_value.json"
);
json_test_suite_test!(
    n_structure_object_with_comment,
    "n_structure_object_with_comment.json"
);
json_test_suite_test!(
    n_structure_object_with_trailing_garbage,
    "n_structure_object_with_trailing_garbage.json"
);
json_test_suite_test!(
    n_structure_open_array_apostrophe,
    "n_structure_open_array_apostrophe.json"
);
json_test_suite_test!(
    n_structure_open_array_comma,
    "n_structure_open_array_comma.json"
);
json_test_suite_test!(
    n_structure_open_array_object,
    "n_structure_open_array_object.json"
);
json_test_suite_test!(
    n_structure_open_array_open_object,
    "n_structure_open_array_open_object.json"
);
json_test_suite_test!(
    n_structure_open_array_open_string,
    "n_structure_open_array_open_string.json"
);
json_test_suite_test!(
    n_structure_open_array_string,
    "n_structure_open_array_string.json"
);
json_test_suite_test!(n_structure_open_object, "n_structure_open_object.json");
json_test_suite_test!(
    n_structure_open_object_close_array,
    "n_structure_open_object_close_array.json"
);
json_test_suite_test!(
    n_structure_open_object_comma,
    "n_structure_open_object_comma.json"
);
json_test_suite_test!(
    n_structure_open_object_open_array,
    "n_structure_open_object_open_array.json"
);
json_test_suite_test!(
    n_structure_open_object_open_string,
    "n_structure_open_object_open_string.json"
);
json_test_suite_test!(
    n_structure_open_object_string_with_apostrophes,
    "n_structure_open_object_string_with_apostrophes.json"
);
json_test_suite_test!(n_structure_open_open, "n_structure_open_open.json");
json_test_suite_test!(n_structure_single_eacute, "n_structure_single_eacute.json");
json_test_suite_test!(n_structure_single_star, "n_structure_single_star.json");
json_test_suite_test!(n_structure_trailing_pound, "n_structure_trailing_#.json");
json_test_suite_test!(
    n_structure_uescaped_LF_before_string,
    "n_structure_uescaped_LF_before_string.json"
);
json_test_suite_test!(
    n_structure_unclosed_array,
    "n_structure_unclosed_array.json"
);
json_test_suite_test!(
    n_structure_unclosed_array_partial_null,
    "n_structure_unclosed_array_partial_null.json"
);
json_test_suite_test!(
    n_structure_unclosed_array_unfinished_false,
    "n_structure_unclosed_array_unfinished_false.json"
);
json_test_suite_test!(
    n_structure_unclosed_array_unfinished_true,
    "n_structure_unclosed_array_unfinished_true.json"
);
json_test_suite_test!(
    n_structure_unclosed_object,
    "n_structure_unclosed_object.json"
);
json_test_suite_test!(
    n_structure_unicode_identifier,
    "n_structure_unicode-identifier.json"
);
json_test_suite_test!(
    n_structure_whitespace_Uplus2060_word_joiner,
    "n_structure_whitespace_U+2060_word_joiner.json"
);
json_test_suite_test!(
    n_structure_whitespace_formfeed,
    "n_structure_whitespace_formfeed.json"
);
json_test_suite_test!(y_array_arraysWithSpaces, "y_array_arraysWithSpaces.json");
json_test_suite_test!(y_array_empty_string, "y_array_empty-string.json");
json_test_suite_test!(y_array_empty, "y_array_empty.json");
json_test_suite_test!(
    y_array_ending_with_newline,
    "y_array_ending_with_newline.json"
);
json_test_suite_test!(y_array_false, "y_array_false.json");
json_test_suite_test!(y_array_heterogeneous, "y_array_heterogeneous.json");
json_test_suite_test!(y_array_null, "y_array_null.json");
json_test_suite_test!(
    y_array_with_1_and_newline,
    "y_array_with_1_and_newline.json"
);
json_test_suite_test!(
    y_array_with_leading_space,
    "y_array_with_leading_space.json"
);
json_test_suite_test!(y_array_with_several_null, "y_array_with_several_null.json");
json_test_suite_test!(
    y_array_with_trailing_space,
    "y_array_with_trailing_space.json"
);
json_test_suite_test!(y_number, "y_number.json");
json_test_suite_test!(y_number_0eplus1, "y_number_0e+1.json");
json_test_suite_test!(y_number_0e1, "y_number_0e1.json");
json_test_suite_test!(y_number_after_space, "y_number_after_space.json");
json_test_suite_test!(
    y_number_double_close_to_zero,
    "y_number_double_close_to_zero.json"
);
json_test_suite_test!(y_number_int_with_exp, "y_number_int_with_exp.json");
json_test_suite_test!(y_number_minus_zero, "y_number_minus_zero.json");
json_test_suite_test!(y_number_negative_int, "y_number_negative_int.json");
json_test_suite_test!(y_number_negative_one, "y_number_negative_one.json");
json_test_suite_test!(y_number_negative_zero, "y_number_negative_zero.json");
json_test_suite_test!(y_number_real_capital_e, "y_number_real_capital_e.json");
json_test_suite_test!(
    y_number_real_capital_e_neg_exp,
    "y_number_real_capital_e_neg_exp.json"
);
json_test_suite_test!(
    y_number_real_capital_e_pos_exp,
    "y_number_real_capital_e_pos_exp.json"
);
json_test_suite_test!(y_number_real_exponent, "y_number_real_exponent.json");
json_test_suite_test!(
    y_number_real_fraction_exponent,
    "y_number_real_fraction_exponent.json"
);
json_test_suite_test!(y_number_real_neg_exp, "y_number_real_neg_exp.json");
json_test_suite_test!(
    y_number_real_pos_exponent,
    "y_number_real_pos_exponent.json"
);
json_test_suite_test!(y_number_simple_int, "y_number_simple_int.json");
json_test_suite_test!(y_number_simple_real, "y_number_simple_real.json");
json_test_suite_test!(y_object, "y_object.json");
json_test_suite_test!(y_object_basic, "y_object_basic.json");
json_test_suite_test!(y_object_duplicated_key, "y_object_duplicated_key.json");
json_test_suite_test!(
    y_object_duplicated_key_and_value,
    "y_object_duplicated_key_and_value.json"
);
json_test_suite_test!(y_object_empty, "y_object_empty.json");
json_test_suite_test!(y_object_empty_key, "y_object_empty_key.json");
json_test_suite_test!(
    y_object_escaped_null_in_key,
    "y_object_escaped_null_in_key.json"
);
json_test_suite_test!(y_object_extreme_numbers, "y_object_extreme_numbers.json");
json_test_suite_test!(y_object_long_strings, "y_object_long_strings.json");
json_test_suite_test!(y_object_simple, "y_object_simple.json");
json_test_suite_test!(y_object_string_unicode, "y_object_string_unicode.json");
json_test_suite_test!(y_object_with_newlines, "y_object_with_newlines.json");
json_test_suite_test!(
    y_string_1_2_3_bytes_UTF_8_sequences,
    "y_string_1_2_3_bytes_UTF-8_sequences.json"
);
json_test_suite_test!(
    y_string_accepted_surrogate_pair,
    "y_string_accepted_surrogate_pair.json"
);
json_test_suite_test!(
    y_string_accepted_surrogate_pairs,
    "y_string_accepted_surrogate_pairs.json"
);
json_test_suite_test!(y_string_allowed_escapes, "y_string_allowed_escapes.json");
json_test_suite_test!(
    y_string_backslash_and_u_escaped_zero,
    "y_string_backslash_and_u_escaped_zero.json"
);
json_test_suite_test!(
    y_string_backslash_doublequotes,
    "y_string_backslash_doublequotes.json"
);
json_test_suite_test!(y_string_comments, "y_string_comments.json");
json_test_suite_test!(y_string_double_escape_a, "y_string_double_escape_a.json");
json_test_suite_test!(y_string_double_escape_n, "y_string_double_escape_n.json");
json_test_suite_test!(
    y_string_escaped_control_character,
    "y_string_escaped_control_character.json"
);
json_test_suite_test!(
    y_string_escaped_noncharacter,
    "y_string_escaped_noncharacter.json"
);
json_test_suite_test!(y_string_in_array, "y_string_in_array.json");
json_test_suite_test!(
    y_string_in_array_with_leading_space,
    "y_string_in_array_with_leading_space.json"
);
json_test_suite_test!(
    y_string_last_surrogates_1_and_2,
    "y_string_last_surrogates_1_and_2.json"
);
json_test_suite_test!(y_string_nbsp_uescaped, "y_string_nbsp_uescaped.json");
json_test_suite_test!(
    y_string_nonCharacterInUTF_8_Uplus10FFFF,
    "y_string_nonCharacterInUTF-8_U+10FFFF.json"
);
json_test_suite_test!(
    y_string_nonCharacterInUTF_8_UplusFFFF,
    "y_string_nonCharacterInUTF-8_U+FFFF.json"
);
json_test_suite_test!(y_string_null_escape, "y_string_null_escape.json");
json_test_suite_test!(y_string_one_byte_utf_8, "y_string_one-byte-utf-8.json");
json_test_suite_test!(y_string_pi, "y_string_pi.json");
json_test_suite_test!(
    y_string_reservedCharacterInUTF_8_Uplus1BFFF,
    "y_string_reservedCharacterInUTF-8_U+1BFFF.json"
);
json_test_suite_test!(y_string_simple_ascii, "y_string_simple_ascii.json");
json_test_suite_test!(y_string_space, "y_string_space.json");
json_test_suite_test!(
    y_string_surrogates_Uplus1D11E_MUSICAL_SYMBOL_G_CLEF,
    "y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF.json"
);
json_test_suite_test!(y_string_three_byte_utf_8, "y_string_three-byte-utf-8.json");
json_test_suite_test!(y_string_two_byte_utf_8, "y_string_two-byte-utf-8.json");
json_test_suite_test!(y_string_uplus2028_line_sep, "y_string_u+2028_line_sep.json");
json_test_suite_test!(y_string_uplus2029_par_sep, "y_string_u+2029_par_sep.json");
json_test_suite_test!(y_string_uEscape, "y_string_uEscape.json");
json_test_suite_test!(y_string_uescaped_newline, "y_string_uescaped_newline.json");
json_test_suite_test!(
    y_string_unescaped_char_delete,
    "y_string_unescaped_char_delete.json"
);
json_test_suite_test!(y_string_unicode, "y_string_unicode.json");
json_test_suite_test!(
    y_string_unicodeEscapedBackslash,
    "y_string_unicodeEscapedBackslash.json"
);
json_test_suite_test!(y_string_unicode_2, "y_string_unicode_2.json");
json_test_suite_test!(
    y_string_unicode_Uplus10FFFE_nonchar,
    "y_string_unicode_U+10FFFE_nonchar.json"
);
json_test_suite_test!(
    y_string_unicode_Uplus1FFFE_nonchar,
    "y_string_unicode_U+1FFFE_nonchar.json"
);
json_test_suite_test!(
    y_string_unicode_Uplus200B_ZERO_WIDTH_SPACE,
    "y_string_unicode_U+200B_ZERO_WIDTH_SPACE.json"
);
json_test_suite_test!(
    y_string_unicode_Uplus2064_invisible_plus,
    "y_string_unicode_U+2064_invisible_plus.json"
);
json_test_suite_test!(
    y_string_unicode_UplusFDD0_nonchar,
    "y_string_unicode_U+FDD0_nonchar.json"
);
json_test_suite_test!(
    y_string_unicode_UplusFFFE_nonchar,
    "y_string_unicode_U+FFFE_nonchar.json"
);
json_test_suite_test!(
    y_string_unicode_escaped_double_quote,
    "y_string_unicode_escaped_double_quote.json"
);
json_test_suite_test!(y_string_utf8, "y_string_utf8.json");
json_test_suite_test!(
    y_string_with_del_character,
    "y_string_with_del_character.json"
);
json_test_suite_test!(y_structure_lonely_false, "y_structure_lonely_false.json");
json_test_suite_test!(y_structure_lonely_int, "y_structure_lonely_int.json");
json_test_suite_test!(
    y_structure_lonely_negative_real,
    "y_structure_lonely_negative_real.json"
);
json_test_suite_test!(y_structure_lonely_null, "y_structure_lonely_null.json");
json_test_suite_test!(y_structure_lonely_string, "y_structure_lonely_string.json");
json_test_suite_test!(y_structure_lonely_true, "y_structure_lonely_true.json");
json_test_suite_test!(y_structure_string_empty, "y_structure_string_empty.json");
json_test_suite_test!(
    y_structure_trailing_newline,
    "y_structure_trailing_newline.json"
);
json_test_suite_test!(y_structure_true_in_array, "y_structure_true_in_array.json");
json_test_suite_test!(
    y_structure_whitespace_array,
    "y_structure_whitespace_array.json"
);
