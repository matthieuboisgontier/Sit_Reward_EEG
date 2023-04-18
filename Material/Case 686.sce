response_matching = simple_matching;
default_text_color = EXPARAM( "Default Font Color" );
default_background_color = EXPARAM( "Default Screen Color" );
default_font_size = EXPARAM( "Default Font Size" );
default_font = EXPARAM( "Default Font" );
write_codes = true;
response_port_output = false;
event_code_delimiter = ";";
stimulus_properties = block, string, left_stim, string, right_stim, string, stim_chosen, string, instruct, string, reward, string; 

active_buttons = 3;

begin;

trial {
	stimulus_event {
		picture {
			text { caption = "WAIT"; };
			x = 0; y = 0;
		} wait_pic;
	} wait_event;
} wait_trial;

trial {
	trial_type = specific_response;
	trial_duration = forever;
	terminator_button = 3;
	
	stimulus_event {
		picture {
			text { caption = "CLICK THE BOTTOM BUTTON TO BEGIN THE NEXT TRIAL"; };
			x = 0; y = 0;
		} press_pic;
	} press_event;
} press_trial;

trial {
	stimulus_event {
		picture {
			text { caption = "+"; font_size = EXPARAM( "Fixation Size" ); } fix_text;
			x = 0; y = 0;
		} fix_pic;
	} fix_event;
} fix_trial;

array {
	box { color = 255, 0, 0; width = EXPARAM( "Square Size" ); height = EXPARAM( "Square Size" ); } left_square;
	box { color = 255, 0, 0; width = EXPARAM( "Square Size" ); height = EXPARAM( "Square Size" ); } right_square;
} squares;

$offset = EXPARAM( "Square Offset" );
trial {
	all_responses = false;	
	trial_type = specific_response;
	trial_duration = forever;
	terminator_button = 1, 2;
	stimulus_event {
		picture {
			box left_square;
			x = '-1 * $offset'; y = 0;
			box right_square;
			x = $offset; y = 0;
			text fix_text;
			x = 0; y = 0;
		} stim_pic;
		response_active = true;
	} stim_event;
} stim_trial;

trial {
	stimulus_event {
		picture {
			bitmap { preload = false; };
			x = 0; y = 0;
		} instruct_pic;
	} instruct_event;
} instruct_trial;

array {
	bitmap { filename = "sit.jpg"; } sit_bmp;
	bitmap { filename = "stand.jpg"; } stand_bmp;
} instruct_bitmaps;

trial {
	stimulus_event {
		picture {
			text { caption = " "; font_size = EXPARAM( "Reward Size" ); } reward_text;
			x = 0; y = 0;
		} reward_pic;
	} reward_event;
} reward_trial;


trial {
	stimulus_event {
		sound { wavefile { preload = false; } tone; } tone_sound;
	} tone_event;
	picture fix_pic;
} tone_trial;

begin_pcl;

#Fixes picture durations to be half a refresh shorter than given
sub int refresh_fix( int dur )
begin
	dur = dur - int( display_device.refresh_period() / 2.0 );
	if dur <= display_device.refresh_period() then
		dur = trial::STIMULI_LENGTH
	end;
	return dur;
end;

#Obtain information from parameter manager
int num_blocks = parameter_manager.get_int( "Number of Blocks" );
int trials_per_block = parameter_manager.get_int( "Trials per Block" );
array<double> perc_sit_list[0];
parameter_manager.get_doubles( "Percentage Sit by Stim", perc_sit_list );
if perc_sit_list.count() != 2 then
	exit( "Percentage Sit by Block parameter should have 2 items" );
end;
array<int> fix_durs[0];
parameter_manager.get_ints( "Fixation Duration Range", fix_durs );
if fix_durs.count() != 2 then
	exit( "Fixation Duration represents a range and therefore should have 2 values" );
end;
double tone_freq = parameter_manager.get_double( "Tone Frequency" );
int tone_dur = parameter_manager.get_int( "Tone Duration" );
double tone_ramp = parameter_manager.get_double( "Percentage Ramping Time Sound" ) / 100.;
int tone_isi = parameter_manager.get_int( "Tone ISI" );
int tone_reps = parameter_manager.get_int( "Number of Tone Repeats" );
tone_trial.set_duration( refresh_fix( tone_isi ) );
int SIT = 1;
int STAND = 2;
array<string> instruct_codes[] = { "sit;", "stand;" };
int wait_dur = parameter_manager.get_int( "Wait Duration" );
array<rgb_color> square_colors[0][0];
parameter_manager.get_colors( "Square Colors by Block", square_colors ); 
#if square_colors.count() != num_blocks then
	#exit( "You must specify the same number of sets of square colors as there are blocks" );
#end;
loop int i = 1 until i >square_colors.count()
begin
	if square_colors[i].count() != 2 then
		exit( "Each subarray in 'Square Colors by Block' must have two colors" );
	end;
	i = i + 1;
end;
instruct_trial.set_duration( refresh_fix( parameter_manager.get_int( "Instruction Duration" ) ) );
reward_trial.set_duration( refresh_fix( parameter_manager.get_int( "Reward Duration" ) ) );

#Create tone stimulus
asg::line ramp_up = new asg::line( tone_dur * tone_ramp, 0., 1. );
asg::line ramp_down = new asg::line( tone_dur * tone_ramp, 1., 0. );
asg::sine sine_wave = new asg::sine( tone_dur, tone_freq, 0. );
asg::waveform_data wf_data = new asg::waveform_data( sine_wave );
wf_data.multiply_segment( ramp_up, 0. );
wf_data.multiply_segment( ramp_down, wf_data.duration() - ( tone_dur * tone_ramp ) );
tone = new wavefile( wf_data );
tone_sound = new sound( tone );
tone_event.set_stimulus( tone_sound );

#-----------------------------------------------------------------------------

sub show_fixation
begin
	fix_trial.set_duration( refresh_fix( random( fix_durs[1], fix_durs[2] ) ) );
	fix_trial.present();
end;

sub run_trial( int block )
begin
	response_manager.set_button_active(3,true);
	press_trial.present();
	response_manager.set_button_active(3,false);
	show_fixation();
	string code = string( block ) + ";";
	array<int> sides[] = { 1, 2 };
	sides.shuffle();
	loop int i = 1 until i > sides.count()
	begin
		stim_pic.set_part( i, squares[sides[i]] );
		code = code + string( sides[i] ) + ";";
		i = i + 1;
	end;
	stim_event.set_event_code( code );
	stim_trial.present();
	int resp = sides[stimulus_manager.last_stimulus_data().button()];
	int instruct = SIT;
	if random() > perc_sit_list[ resp ] / 100.0 then
		instruct = STAND;
	end;
	show_fixation();
	instruct_pic.set_part( 1, instruct_bitmaps[instruct] );
	instruct_event.set_port_code( instruct );
	code = code + string( resp ) + ";" + instruct_codes[instruct];
	instruct_event.set_event_code( code );
	instruct_trial.present();
	show_fixation();
	bool reward = bool( random( 0, 1 ) );
	if reward then
		reward_text.set_caption( "$", true );
	else
		reward_text.set_caption( "0", true );
	end;
	reward_event.set_port_code( int( string( instruct ) + string( int( reward ) ) ) );
	code = code + string( reward ) + ";";
	reward_event.set_event_code( code );
	reward_trial.present();
	if reward then
		wait_trial.set_duration( refresh_fix( wait_dur ) );
		wait_trial.present();
		loop int i = 1 until i > tone_reps
		begin
			tone_trial.present();
			i = i + 1;
		end;
	else
		wait_trial.set_duration( refresh_fix( wait_dur + ( tone_reps * tone_isi ) ) );
		wait_trial.present();
	end;
end;

#-MAIN LOOP------------------------------------------
loop int i = 1 until i > num_blocks
begin
	loop int k = 1 until k > squares.count()
	begin
		squares[k].set_color( square_colors[i][k] );
		k = k + 1;
	end;

	loop int j = 1 until j > trials_per_block
	begin
		run_trial( i );
		j = j + 1;
	end;
	i = i + 1;
end;


