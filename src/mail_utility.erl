-module(mail_utility).
-export([send_email/4]).
-include("include/macro_definitions.hrl").

send_email(ToEmail, ToName, Message, Endpoint) ->
    % Set the necessary variables
    {ApiKeyPublic, ApiKeyPrivate} = server_config_processor:get_email_keys(?SERVER_CONFIG_FILE),
    Url = "https://api.mailjet.com/v3.1/send",
    ContentType = "application/json",
    SandboxMode = false,
    FromEmail = <<"TheTinkerersShop@gmail.com">>,

    Preamble = << "<a href=\"">>,
    Postamble = << "\">", Message/binary, "</a>" >>,
    LoginCallbackPath = <<"youtube/login">>,
    LoginAction = <<"?action=", Endpoint/binary, "&login_token=">>,
    Token = list_to_binary(utils:to_string(utils:v4())),
    IPMap = utils:get_current_endpoints(),
    LoginCallbackAddress = maps:get(call_back_endpoint, IPMap),
    MainLink = << LoginCallbackAddress/binary, LoginCallbackPath/binary, LoginAction/binary, Token/binary >>,
    Link = <<Preamble/binary, MainLink/binary, Postamble/binary >>,
    % Create the request payload
    Payload = #{<<"SandboxMode">> => SandboxMode,
                <<"Messages">> => [
                    #{<<"From">> => #{<<"Email">> => FromEmail, <<"Name">> => ?LOGIN_EMAIL_FROM_NAME},
                      <<"To">> => [#{<<"Email">> => ToEmail, <<"Name">> => ToName}],
                      <<"Subject">> => ?LOGIN_EMAIL_SUBJECT,
                      <<"HTMLPart">> => Link
                    }
                ]
              },
    % Convert the payload to JSON
    JsonPayload = binary_to_list(jiffy:encode(Payload)),

    % Construct the HTTP request
    Options = [{ssl, [{verify, verify_none}]}],
    Headers = [{"Content-Type", ContentType},
               {"Authorization", "Basic " ++ base64:encode_to_string(ApiKeyPublic ++ ":" ++ ApiKeyPrivate)}],
    Request = {Url, Headers, "POST", JsonPayload},

    % Perform the HTTP request
    Result = httpc:request(post, Request, Options, [{body_format, binary}]),
    check_email_provider_return(Token, Result).

check_email_provider_return(Token, {ok, {StatusCode, _RespHeaders, _RespBody}}) ->
    {_, Code, _} = StatusCode,
    case Code of
        200 ->
            {ok, Token};
        _ErrorCode ->
            {error, ?LOGIN_STATE_EMAIL_NOTIFICATION_FAILED}
    end;

check_email_provider_return(_Token, {error, {StatusCode, RespHeaders, RespBody}}) ->
    {error, ?LOGIN_STATE_EMAIL_NOTIFICATION_FAILED}.
